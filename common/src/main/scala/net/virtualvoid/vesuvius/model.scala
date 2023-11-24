package net.virtualvoid.vesuvius

case class SegmentReference(scrollRef: ScrollReference, segmentId: String) {
  def scrollId: String = scrollRef.scrollId
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

case class ScrollReference(scrollId: String, base: ScrollServerBase, defaultVolumeId: String) {
  def baseUrl: String = base.baseUrl(scrollId)
  def scrollUrl: String = base.scrollUrl(scrollId)
  def volumeMetadataUrl(volumeId: String): String = s"${volumeUrl(volumeId)}meta.json"
  def volumeUrl(volumeId: String): String = s"${scrollUrl}volumes/$volumeId/"
  def volumeGridUrl(volumeId: String): String = s"${scrollUrl}volume_grids/$volumeId/"
}

object ScrollReference {
  val scrolls: Seq[ScrollReference] = Seq(
    ScrollReference("1", FullScrollsBase, "20230205180739"),
    ScrollReference("2", FullScrollsBase, "20230210143520"),
    ScrollReference("0332", PHercBase, "20231027191953"),
    ScrollReference("1667", PHercBase, "20231107190228")
  )

  def byId(id: Int): Option[ScrollReference] =
    scrolls.find(_.scrollId == id.toString)

  def byId(id: String): Option[ScrollReference] =
    scrolls.find(_.scrollId == id)
}

sealed trait ScrollServerBase extends Product {
  def scrollUrl(scrollId: String): String

  def baseUrl(scrollId: String): String = s"${scrollUrl(scrollId)}paths/"
  def segmentUrl(segment: SegmentReference): String =
    s"${baseUrl(segment.scrollId)}${segment.segmentId}/"

  def layerUrl(segment: SegmentReference, z: Int): String =
    f"${segmentUrl(segment)}layers/$z%02d.tif"
}

case object FullScrollsBase extends ScrollServerBase {

  def scrollUrl(scroll: String): String =
    s"http://dl.ash2txt.org/full-scrolls/Scroll$scroll.volpkg/"
}

case object PHercBase extends ScrollServerBase {
  def scrollUrl(scroll: String): String =
    f"http://dl.ash2txt.org/full-scrolls/PHerc$scroll.volpkg/"

  override def layerUrl(segment: SegmentReference, z: Int): String =
    f"${segmentUrl(segment)}layers/$z%03d.tif"
}
case class ImageInfo(
    ref:    SegmentReference,
    width:  Int,
    height: Int,
    area:   Option[Float]
) {
  def scrollId: String = ref.scrollId
  def segmentId: String = ref.segmentId

  def isLandscape: Boolean = width > height
}