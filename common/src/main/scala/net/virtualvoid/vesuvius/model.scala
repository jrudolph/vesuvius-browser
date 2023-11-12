package net.virtualvoid.vesuvius

case class SegmentReference(scrollRef: ScrollReference, segmentId: String) {
  def scroll: Int = scrollRef.scroll
  def base: ScrollServerBase = scrollRef.base
  def baseUrl: String = base.segmentUrl(this)

  def layerUrl(z: Int): String = base.layerUrl(this, z)
}
object SegmentReference {
  import spray.json._
  import DefaultJsonProtocol._

  implicit val scrollServerBaseFormat: RootJsonFormat[ScrollServerBase] =
    new RootJsonFormat[ScrollServerBase] {
      def write(obj: ScrollServerBase): JsValue = JsString(obj.productPrefix)
      def read(json: JsValue): ScrollServerBase = json.convertTo[String] match {
        case "FullScrollsBase" => FullScrollsBase
        case "PHercBase"       => PHercBase
      }
    }
  implicit val scrollReferenceFormat: RootJsonFormat[ScrollReference] = jsonFormat3(ScrollReference.apply)
  implicit val segmentReferenceFormat: RootJsonFormat[SegmentReference] = jsonFormat2(SegmentReference.apply)
}

case class ScrollReference(scroll: Int, base: ScrollServerBase, defaultVolumeId: String) {
  def baseUrl: String = base.baseUrl(scroll)
  def scrollUrl: String = base.scrollUrl(scroll)
  def volumeMetadataUrl(volumeId: String): String = s"${volumeUrl(volumeId)}meta.json"
  def volumeUrl(volumeId: String): String = s"${scrollUrl}volumes/$volumeId/"
  def volumeGridUrl(volumeId: String): String = s"${scrollUrl}volume_grids/$volumeId/"
}

object ScrollReference {
  val scrolls: Seq[ScrollReference] = Seq(
    ScrollReference(1, FullScrollsBase, "20230205180739"),
    ScrollReference(2, FullScrollsBase, "20230210143520"),
    ScrollReference(332, PHercBase, "20230210143520"),
    ScrollReference(1667, PHercBase, "20231027191953")
  )

  def byId(id: Int): Option[ScrollReference] =
    scrolls.find(_.scroll == id)
}

sealed trait ScrollServerBase extends Product {
  def scrollUrl(scroll: Int): String

  def baseUrl(scroll: Int): String = s"${scrollUrl(scroll)}paths/"
  def segmentUrl(segment: SegmentReference): String =
    s"${baseUrl(segment.scroll)}${segment.segmentId}/"

  def layerUrl(segment: SegmentReference, z: Int): String =
    f"${segmentUrl(segment)}layers/$z%02d.tif"
}

case object FullScrollsBase extends ScrollServerBase {

  def scrollUrl(scroll: Int): String =
    s"http://dl.ash2txt.org/full-scrolls/Scroll$scroll.volpkg/"
}

case object PHercBase extends ScrollServerBase {
  def scrollUrl(scroll: Int): String =
    f"http://dl.ash2txt.org/full-scrolls/PHerc$scroll%04d.volpkg/"

  override def layerUrl(segment: SegmentReference, z: Int): String =
    f"${segmentUrl(segment)}layers/$z%03d.tif"
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