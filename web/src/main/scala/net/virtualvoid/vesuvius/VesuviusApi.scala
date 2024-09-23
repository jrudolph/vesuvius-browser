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

case class SegmentInfo(
    scroll: ScrollId,
    id:     String,
    width:  Int,
    height: Int
)

object SegmentInfo {
  import DefaultJsonProtocol._

  implicit val segmentInfoJsonFormat: JsonFormat[SegmentInfo] = jsonFormat4(SegmentInfo.apply)

  def fromImageInfo(info: ImageInfo): SegmentInfo =
    SegmentInfo(ScrollId(info.ref.newScrollId.name, info.ref.newScrollId.number.toString), info.ref.segmentId, info.width, info.height)
}

trait VesuviusApi { self: VesuviusRoutes =>
  import system.dispatcher
  import DefaultJsonProtocol._

  private lazy val helloWorldEndpoint = endpoint.get.in("api" / "segments").out(jsonBody[Seq[SegmentInfo]])

  private lazy val swaggerEndpoints = SwaggerInterpreter()
    .fromEndpoints[Future](List(helloWorldEndpoint), "Vesuvius Browser API", "1.0")

  private lazy val allEndpoints: List[ServerEndpoint[Any, Future]] =
    swaggerEndpoints :+ helloWorldEndpoint.serverLogicSuccess[Future] { input =>
      scrollSegments.map(_.map(SegmentInfo.fromImageInfo))
    }

  lazy val apiRoutes =
    PekkoHttpServerInterpreter().toRoute(allEndpoints)
}
