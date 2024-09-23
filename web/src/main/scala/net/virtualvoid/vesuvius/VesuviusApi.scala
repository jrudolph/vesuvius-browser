package net.virtualvoid.vesuvius

import spray.json._
import sttp.tapir._
import sttp.tapir.generic.auto._
import sttp.tapir.json.spray._
import sttp.tapir.server.pekkohttp.PekkoHttpServerInterpreter
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import scala.concurrent.Future

case class ScrollId(
    id:  String,
    num: String
)
object ScrollId {
  import DefaultJsonProtocol._

  implicit val scrollIdJsonFormat: JsonFormat[ScrollId] = jsonFormat2(ScrollId.apply)
}

case class SegmentUrls(
    maskUrl:      String,
    objUrl:       String,
    compositeUrl: String,
    ppmUrl:       String
)
object SegmentUrls {
  import DefaultJsonProtocol._

  implicit val segmentUrlsJsonFormat: JsonFormat[SegmentUrls] = jsonFormat4(SegmentUrls.apply)
}

case class SegmentInfo(
    scroll:  ScrollId,
    id:      String,
    width:   Int,
    height:  Int,
    urls:    SegmentUrls,
    areaCm2: Option[Float]
)

object SegmentInfo {
  import DefaultJsonProtocol._

  implicit val segmentInfoJsonFormat: JsonFormat[SegmentInfo] = jsonFormat6(SegmentInfo.apply)

  def fromImageInfo(info: ImageInfo): SegmentInfo =
    SegmentInfo(
      ScrollId(info.ref.newScrollId.name, info.ref.newScrollId.number.toString),
      info.ref.segmentId,
      info.width,
      info.height,
      SegmentUrls(
        info.ref.maskUrl,
        info.ref.objUrl,
        info.ref.compositeUrl,
        info.ref.ppmUrl
      ),
      info.area)
}

trait VesuviusApi { self: VesuviusRoutes =>
  import system.dispatcher
  import DefaultJsonProtocol._

  private lazy val catalogEndpoint = endpoint.get.in("api" / "catalog").out(jsonBody[Seq[SegmentInfo]])

  private lazy val swaggerEndpoints = SwaggerInterpreter()
    .fromEndpoints[Future](List(catalogEndpoint), "Vesuvius Browser API", "1.0")

  private lazy val allEndpoints: List[ServerEndpoint[Any, Future]] =
    swaggerEndpoints :+ catalogEndpoint.serverLogicSuccess[Future] { input =>
      scrollSegments.map(_.map(SegmentInfo.fromImageInfo))
    }

  lazy val apiRoutes =
    PekkoHttpServerInterpreter().toRoute(allEndpoints)
}
