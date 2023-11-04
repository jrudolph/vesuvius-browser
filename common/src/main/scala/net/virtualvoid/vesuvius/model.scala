package net.virtualvoid.vesuvius

case class SegmentReference(scroll: Int, segmentId: String, base: ScrollServerBase) {
  def baseUrl: String = base.segmentUrl(this)

  def layerUrl(z: Int): String = base.layerUrl(this, z)
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
  def segmentUrl(segment: SegmentReference): String =
    s"${baseUrl(segment.scroll)}${segment.segmentId}/"

  def layerUrl(segment: SegmentReference, z: Int): String =
    f"${segmentUrl(segment)}layers/$z%02d.tif"
}

case object FullScrollsBase extends ScrollServerBase {
  def baseUrl(scroll: Int): String =
    s"http://dl.ash2txt.org/full-scrolls/Scroll$scroll.volpkg/paths/"
}

case object PHercBase extends ScrollServerBase {
  def baseUrl(scroll: Int): String =
    f"http://dl.ash2txt.org/full-scrolls/PHerc$scroll%04d.volpkg/paths/"

  override def layerUrl(segment: SegmentReference, z: Int): String =
    f"${segmentUrl(segment)}layers/$z%03d.tif"
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

  def isLandscape: Boolean = width > height
}