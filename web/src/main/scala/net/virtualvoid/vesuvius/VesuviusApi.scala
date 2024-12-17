package net.virtualvoid.vesuvius

import net.virtualvoid.vesuvius
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.StatusCodes
import spray.json.*
import sttp.apispec.openapi.Server
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.spray.*
import sttp.tapir.server.pekkohttp.PekkoHttpServerInterpreter
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import scala.concurrent.Future
import scala.concurrent.duration._

import java.io.File

trait VesuviusApi { //self: VesuviusRoutes =>
  val system: ActorSystem
  def scrollSegments: Future[Seq[SegmentInfo]]
  def downloadUtils: DownloadUtils
  def layersFor(segment: SegmentReference): Future[Seq[String]]
  def config: AppConfig
  def dataDir: File = config.dataDir

  val SegmentsDataVersion = 7

  import system.dispatcher
  import DefaultJsonProtocol._

  private lazy val versionEndpoint =
    endpoint.get
      .in("version")
      .out(jsonBody[VesuviusApi.BackendVersion])
      .description("Get backend server version information")

  private lazy val catalogEndpoint =
    endpoint.get
      .in("segments")
      .out(jsonBody[Seq[VesuviusApi.SegmentInfo]])
      .description("Get all segments in the catalog")

  private lazy val segmentInfoEndpoint =
    endpoint.get
      .in("segments" / "scroll" / path[String]("scrollId") / path[String]("segmentId"))
      .out(jsonBody[VesuviusApi.SegmentInfo])
      .description("Get information about a specific segment")

  private lazy val segmentReportEndpoint =
    endpoint.get
      .in("segments" / "url-report")
      .out(jsonBody[Seq[VesuviusApi.SegmentReport]])
      .description("Reports state of data on the data server for each segment")

  private lazy val swaggerEndpoints =
    SwaggerInterpreter(
      customiseDocsModel = { api =>
        api.copy(servers = List(Server("/api")))
      }
    )
      .fromEndpoints[Future](
        List(versionEndpoint, catalogEndpoint, segmentInfoEndpoint, segmentReportEndpoint),
        "Vesuvius Browser API",
        "1.0"
      )

  private lazy val versionImplementation =
    versionEndpoint.serverLogicSuccess[Future] { _ =>
      Future.successful(
        VesuviusApi.BackendVersion(
          BuildInfo.buildVersion,
          BuildInfo.gitCommit,
          BuildInfo.builtAtMillis
        )
      )
    }

  def calculateSegments(): Future[Seq[VesuviusApi.SegmentInfo]] =
    for {
      segments <- scrollSegments
      result <- Future.traverse(segments) { s =>
        layersFor(s.ref).map {
          VesuviusApi.SegmentInfo.fromImageInfo(s, _)
        }
      }
    } yield result

  def cachedSegments(): Future[Seq[VesuviusApi.SegmentInfo]] =
    fromApiCache[Seq[VesuviusApi.SegmentInfo]]("segments", SegmentsDataVersion)

  lazy val ApiCache = downloadUtils.jsonCache[(String, Int), JsValue](
    { case (what, version) => new File(dataDir, s"cache/api/$what-$version.json") },
    ttl = 6.hours,
    negativeTtl = 10.seconds
  ) {
      case ("segments", _)   => calculateSegments().map(_.toJson)
      case ("url-report", _) => getUrlReport.map(_.toJson)
    }

  def fromApiCache[T: JsonFormat](what: String, version: Int): Future[T] =
    ApiCache(what -> version).map(_.convertTo[T])

  private lazy val catalogImplementation =
    catalogEndpoint.serverLogicSuccess[Future] { _ => cachedSegments() }

  private lazy val urlReportCache = downloadUtils.jsonCache[(SegmentReference, String, String), VesuviusApi.UrlReport](
    { case (segment, what, _) => new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/$what-report.json") }
  ) {
      case (_, _, url) =>
        println(s"Checking $url")
        downloadUtils.head(url).map { res =>
          if (res.status == StatusCodes.OK) VesuviusApi.Status.Ok(res.entity.contentLengthOption.getOrElse(0L))
          else if (res.status == StatusCodes.NotFound)
            VesuviusApi.Status.NotFound
          else
            throw new RuntimeException(s"Unexpected status code: ${res.status}")
        }.map(VesuviusApi.UrlReport(url, _))

    }

  private def reportFor(segment: SegmentReference): Future[VesuviusApi.SegmentReport] = {
    def urlReportFor(segment: SegmentReference, what: String, url: String): Future[VesuviusApi.UrlReport] =
      urlReportCache((segment, what, url))

    for {
      mask <- urlReportFor(segment, "mask", segment.maskUrl)
      area <- urlReportFor(segment, "area", segment.areaUrl)
      obj <- urlReportFor(segment, "obj", segment.objUrl)
      ppm <- urlReportFor(segment, "ppm", segment.ppmUrl)
      meta <- urlReportFor(segment, "meta", segment.metaUrl)
      author <- urlReportFor(segment, "author", segment.authorUrl)
      composite <- urlReportFor(segment, "composite", segment.compositeUrl)
      layer0 <- urlReportFor(segment, "layer0", segment.layerUrl(0))
      layer32 <- urlReportFor(segment, "layer32", segment.layerUrl(32))
    } yield VesuviusApi.SegmentReport(
      scroll = VesuviusApi.ScrollId.forRef(segment),
      id = segment.segmentId,
      baseUrl = segment.baseUrl,
      mask = mask,
      area = area,
      obj = obj,
      ppm = ppm,
      meta = meta,
      author = author,
      composite = composite,
      layer0 = layer0,
      layer32 = layer32
    )
  }

  def getUrlReport: Future[Seq[VesuviusApi.SegmentReport]] =
    for {
      segments <- scrollSegments
      result <- Future.traverse(segments) { s =>
        reportFor(s.ref)
      }
    } yield result

  private lazy val segmentReportImplementation =
    segmentReportEndpoint.serverLogicSuccess[Future] { _ =>
      fromApiCache[Seq[VesuviusApi.SegmentReport]]("url-report", 1)
    }

  private lazy val segmentInfoImplementation =
    segmentInfoEndpoint.serverLogicSuccess[Future] { (scrollId, segmentId) =>
      cachedSegments().map { segments =>
        segments.find(i => i.id == segmentId && i.scroll.oldId == scrollId)
          .getOrElse(throw new RuntimeException(s"Segment $segmentId not found"))
      }
    }

  private lazy val allEndpoints: List[ServerEndpoint[Any, Future]] =
    swaggerEndpoints :+ catalogImplementation :+ segmentReportImplementation :+ segmentInfoImplementation :+ versionImplementation

  lazy val apiRoutes =
    PekkoHttpServerInterpreter().toRoute(allEndpoints)
}

object VesuviusApi {
  case class ScrollId(
      id:         String,
      num:        String,
      oldId:      String,
      isFragment: Boolean
  )

  object ScrollId {
    import DefaultJsonProtocol._

    implicit val scrollIdJsonFormat: JsonFormat[ScrollId] = jsonFormat4(
      ScrollId.apply
    )

    def forRef(ref: vesuvius.SegmentReference): ScrollId =
      ScrollId(
        ref.newScrollId.name,
        ref.newScrollId.number.toString,
        oldId = ref.scrollRef.scrollId,
        isFragment = ref.scrollRef.isFragment
      )
  }

  case class SegmentUrls(
      baseUrl:      String,
      maskUrl:      String,
      metaUrl:      String,
      objUrl:       String,
      compositeUrl: String,
      ppmUrl:       String,
      authorUrl:    String
  )

  object SegmentUrls {
    import DefaultJsonProtocol._

    implicit val segmentUrlsJsonFormat: JsonFormat[SegmentUrls] = jsonFormat7(
      SegmentUrls.apply
    )
  }

  case class VolumeInfo(
      volume:      String,
      baseUrl:     String,
      voxelSizenM: Int,
      energykeV:   Int)
  object VolumeInfo {
    import DefaultJsonProtocol._

    implicit val volumeInfoJsonFormat: JsonFormat[VolumeInfo] = jsonFormat4(VolumeInfo.apply)

    def fromMetadata(meta: vesuvius.VolumeMetadata, url: String): VolumeInfo =
      VolumeInfo(
        meta.uuid,
        url,
        (meta.voxelsize * 1000).toInt,
        meta.energykeV
      )
  }

  case class SegmentInfo(
      scroll:  ScrollId,
      id:      String,
      width:   Int,
      height:  Int,
      minZ:    Option[Int],
      maxZ:    Option[Int],
      volume:  Option[VolumeInfo],
      urls:    SegmentUrls,
      areaCm2: Option[Float],
      author:  Option[String],
      layers:  Seq[String]
  )

  object SegmentInfo {
    import DefaultJsonProtocol._

    implicit val segmentInfoJsonFormat: JsonFormat[SegmentInfo] = jsonFormat11(
      SegmentInfo.apply
    )

    def fromImageInfo(
      info:   vesuvius.SegmentInfo,
      layers: Seq[String]
    ): SegmentInfo =
      SegmentInfo(
        scroll = ScrollId.forRef(info.ref),
        id = info.ref.segmentId,
        width = info.width,
        height = info.height,
        minZ = info.minZ,
        maxZ = info.maxZ,
        volume = info.volumeMetadata.map(v => VolumeInfo.fromMetadata(v, info.ref.scrollRef.volumeUrl(v.uuid))),
        urls = SegmentUrls(
          info.ref.baseUrl,
          info.ref.maskUrl,
          info.ref.metaUrl,
          info.ref.objUrl,
          info.ref.compositeUrl,
          info.ref.ppmUrl,
          info.ref.authorUrl
        ),
        areaCm2 = info.area,
        author = info.author,
        layers = layers
      )
  }

  sealed trait Status
  object Status {
    case class Ok(size: Long) extends Status
    case object NotFound extends Status

    import DefaultJsonProtocol._
    implicit val okFormat: JsonFormat[Ok] = jsonFormat1(Ok.apply)
    implicit val statusFormat: JsonFormat[Status] = new JsonFormat[Status] {
      override def write(obj: Status): JsValue = obj match {
        case Ok(size) =>
          JsObject("status" -> JsString("ok"), "size" -> JsNumber(size))
        case NotFound => JsObject("status" -> JsString("not-found"))
      }

      override def read(json: JsValue): Status =
        json.asJsObject.fields("status") match {
          case JsString("ok")        => json.convertTo[Ok]
          case JsString("not-found") => NotFound
          case _                     => throw new IllegalArgumentException("Invalid status")
        }
    }
  }
  case class UrlReport(
      url:    String,
      status: Status
  )
  object UrlReport {
    import DefaultJsonProtocol._

    implicit val urlReportJsonFormat: JsonFormat[UrlReport] = jsonFormat2(
      UrlReport.apply
    )
  }
  case class SegmentReport(
      scroll:    ScrollId,
      id:        String,
      baseUrl:   String,
      mask:      UrlReport,
      area:      UrlReport,
      obj:       UrlReport,
      ppm:       UrlReport,
      meta:      UrlReport,
      author:    UrlReport,
      composite: UrlReport,
      layer0:    UrlReport,
      layer32:   UrlReport
  )
  object SegmentReport {
    import DefaultJsonProtocol._

    implicit val segmentReportJsonFormat: JsonFormat[SegmentReport] =
      jsonFormat12(SegmentReport.apply)
  }

  case class BackendVersion(
      version:         String,
      commit:          String,
      buildTimeMillis: Long
  )
  object BackendVersion {
    import DefaultJsonProtocol._

    implicit val backendVersionJsonFormat: JsonFormat[BackendVersion] =
      jsonFormat3(BackendVersion.apply)
  }
}
