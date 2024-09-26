package net.virtualvoid.vesuvius

import spray.json.*
import sttp.apispec.openapi.Server
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.spray.*
import sttp.tapir.server.pekkohttp.PekkoHttpServerInterpreter
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter

import scala.concurrent.Future

case class ScrollId(
    id:         String,
    num:        String,
    isFragment: Boolean
)
object ScrollId {
  import DefaultJsonProtocol._

  implicit val scrollIdJsonFormat: JsonFormat[ScrollId] = jsonFormat3(ScrollId.apply)
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

  implicit val segmentUrlsJsonFormat: JsonFormat[SegmentUrls] = jsonFormat5(SegmentUrls.apply)
}

case class SegmentInfo(
    scroll:  ScrollId,
    id:      String,
    width:   Int,
    height:  Int,
    volume:  Option[String],
    urls:    SegmentUrls,
    areaCm2: Option[Float]
)

object SegmentInfo {
  import DefaultJsonProtocol._

  implicit val segmentInfoJsonFormat: JsonFormat[SegmentInfo] = jsonFormat7(SegmentInfo.apply)

  def fromImageInfo(info: ImageInfo): SegmentInfo =
    SegmentInfo(
      ScrollId(info.ref.newScrollId.name, info.ref.newScrollId.number.toString, isFragment = info.ref.scrollRef.isFragment),
      info.ref.segmentId,
      info.width,
      info.height,
      info.metadata.map(_.volume),
      SegmentUrls(
        info.ref.maskUrl,
        info.ref.metaUrl,
        info.ref.objUrl,
        info.ref.compositeUrl,
        info.ref.ppmUrl
      ),
      info.area)
}

trait VesuviusApi { self: VesuviusRoutes =>
  import system.dispatcher
  import DefaultJsonProtocol._

  private lazy val catalogEndpoint =
    endpoint
      .get
      .in("segments")
      .out(jsonBody[Seq[SegmentInfo]])
      .description("Get all segments in the catalog")

  private lazy val swaggerEndpoints =
    SwaggerInterpreter(
      customiseDocsModel = { api =>
        api.copy(servers = List(Server("/api")))
      }
    )
      .fromEndpoints[Future](List(catalogEndpoint), "Vesuvius Browser API", "1.0")

  private lazy val allEndpoints: List[ServerEndpoint[Any, Future]] =
    swaggerEndpoints :+ catalogEndpoint.serverLogicSuccess[Future] { input =>
      scrollSegments.map(_.map(SegmentInfo.fromImageInfo))
    }

  lazy val apiRoutes =
    PekkoHttpServerInterpreter().toRoute(allEndpoints)
}
