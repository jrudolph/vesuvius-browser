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

import java.io.File

trait VesuviusApi { //self: VesuviusRoutes =>
  val system: ActorSystem
  def scrollSegments: Future[Seq[SegmentInfo]]
  def downloadUtils: DownloadUtils
  def layersFor(segment: SegmentReference): Future[Seq[String]]
  def config: AppConfig
  def dataDir: File = config.dataDir

  import system.dispatcher
  import DefaultJsonProtocol._

  private lazy val catalogEndpoint =
    endpoint.get
      .in("segments")
      .out(jsonBody[Seq[VesuviusApi.SegmentInfo]])
      .description("Get all segments in the catalog")

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
        List(catalogEndpoint, segmentReportEndpoint),
        "Vesuvius Browser API",
        "1.0"
      )

  private lazy val catalogImplementation =
    catalogEndpoint.serverLogicSuccess[Future] { _ =>
      for {
        segments <- scrollSegments
        result <- Future.traverse(segments) { s =>
          layersFor(s.ref).map {
            VesuviusApi.SegmentInfo.fromImageInfo(s, _)
          }
        }
      } yield result
    }

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
      composite <- urlReportFor(segment, "composite", segment.compositeUrl)
      layer0 <- urlReportFor(segment, "layer0", segment.layerUrl(0))
      layer32 <- urlReportFor(segment, "layer32", segment.layerUrl(32))
    } yield VesuviusApi.SegmentReport(
      VesuviusApi.ScrollId.forRef(segment),
      segment.segmentId,
      segment.baseUrl,
      mask,
      area,
      obj,
      ppm,
      meta,
      composite,
      layer0,
      layer32
    )
  }

  private lazy val segmentReportImplementation =
    segmentReportEndpoint.serverLogicSuccess[Future] { _ =>
      for {
        segments <- scrollSegments
        result <- Future.traverse(segments) { s =>
          reportFor(s.ref)
        }
      } yield result
    }

  private lazy val allEndpoints: List[ServerEndpoint[Any, Future]] =
    swaggerEndpoints :+ catalogImplementation :+ segmentReportImplementation

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
      maskUrl:      String,
      metaUrl:      String,
      objUrl:       String,
      compositeUrl: String,
      ppmUrl:       String
  )

  object SegmentUrls {
    import DefaultJsonProtocol._

    implicit val segmentUrlsJsonFormat: JsonFormat[SegmentUrls] = jsonFormat5(
      SegmentUrls.apply
    )
  }

  case class SegmentInfo(
      scroll:          ScrollId,
      id:              String,
      width:           Int,
      height:          Int,
      minZ:            Option[Int],
      maxZ:            Option[Int],
      volume:          Option[String],
      volumeVoxelSize: Option[Double],
      urls:            SegmentUrls,
      areaCm2:         Option[Float],
      layers:          Seq[String]
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
        ScrollId.forRef(info.ref),
        info.ref.segmentId,
        info.width,
        info.height,
        info.minZ,
        info.maxZ,
        info.volumeMetadata.map(_.uuid),
        info.volumeMetadata.map(_.voxelsize),
        SegmentUrls(
          info.ref.maskUrl,
          info.ref.metaUrl,
          info.ref.objUrl,
          info.ref.compositeUrl,
          info.ref.ppmUrl
        ),
        info.area,
        layers
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
      composite: UrlReport,
      layer0:    UrlReport,
      layer32:   UrlReport
  )
  object SegmentReport {
    import DefaultJsonProtocol._

    implicit val segmentReportJsonFormat: JsonFormat[SegmentReport] =
      jsonFormat11(SegmentReport.apply)
  }
}
