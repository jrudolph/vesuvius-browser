package net.virtualvoid.vesuvius

case class SegmentReference(scroll: Int, segmentId: String, base: ScrollServerBase) {
  def baseUrl: String = s"${base.baseUrl(scroll)}$segmentId/"
}
object SegmentReference {
  import spray.json._
  import DefaultJsonProtocol._

  implicit val scrollServerBaseFormat: JsonFormat[ScrollServerBase] =
    new JsonFormat[ScrollServerBase] {
      def write(obj: ScrollServerBase): JsValue = JsString(obj.productPrefix)
      def read(json: JsValue): ScrollServerBase = json.convertTo[String] match {
        case "FullScrollsBase" => FullScrollsBase
      }
    }
  implicit val segmentReferenceFormat: JsonFormat[SegmentReference] = jsonFormat3(SegmentReference.apply)
}

sealed trait ScrollServerBase extends Product {
  def baseUrl(scroll: Int): String
}

case object FullScrollsBase extends ScrollServerBase {
  def baseUrl(scroll: Int): String =
    s"http://dl.ash2txt.org/full-scrolls/Scroll$scroll.volpkg/paths/"
}

case object HariSeldonUploadsBase extends ScrollServerBase {
  def baseUrl(scroll: Int): String =
    s"http://dl.ash2txt.org/hari-seldon-uploads/team-finished-paths/scroll$scroll/"
}

case class ImageInfo(
    ref:    SegmentReference,
    width:  Int,
    height: Int,
    area:   Option[Float]
) {
  def scroll: Int = ref.scroll
  def segmentId: String = ref.segmentId
}