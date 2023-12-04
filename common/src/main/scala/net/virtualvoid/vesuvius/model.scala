package net.virtualvoid.vesuvius

case class SegmentReference(scrollRef: ScrollReference, segmentId: String) {
  def scrollId: String = scrollRef.scrollId
  def base: ScrollServerBase = scrollRef.base
  def baseUrl: String = base.segmentUrl(this)

  def layerUrl(z: Int): String = base.layerUrl(this, z)

  def maskUrl: String = base.maskFor(this)
  def inklabelUrl: String = base.inklabelFor(this)
}
object SegmentReference {
  import spray.json._
  import DefaultJsonProtocol._

  implicit val scrollServerBaseFormat: RootJsonFormat[ScrollServerBase] =
    new RootJsonFormat[ScrollServerBase] {
      def write(obj: ScrollServerBase): JsValue = JsString(obj.productPrefix)
      def read(json: JsValue): ScrollServerBase = json.convertTo[String] match {
        case "FullScrollsBase"               => FullScrollsBase
        case "PHercBase"                     => PHercBase
        case "FragmentsBase"                 => FragmentsBase
        case "OldFragmentsBase"              => OldFragmentsBase
        case "PHerc1667Cr01Fr03FragmentBase" => PHerc1667Cr01Fr03FragmentBase
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
    ScrollReference("1667", PHercBase, "20231107190228"),

    ScrollReference("PHerc1667Cr01Fr03", PHerc1667Cr01Fr03FragmentBase, "20231121133215"),
    ScrollReference("PHerc0051Cr04Fr08", FragmentsBase, "20231121152933"),
    ScrollReference("Frag1", OldFragmentsBase, "20230205142449"), // 2nd volume: 20230213100222
    ScrollReference("Frag2", OldFragmentsBase, "20230216174557"), // 2nd volume: 20230226143835
    ScrollReference("Frag3", OldFragmentsBase, "20230212182547"), // 2nd volume: 20230215142309
    ScrollReference("Frag4", OldFragmentsBase, "20230215185642") // 2nd volume: 20230222173037
  )

  def byId(id: Int): Option[ScrollReference] =
    scrolls.find(_.scrollId == id.toString)

  def byId(id: String): Option[ScrollReference] = scrolls.find(_.scrollId == id)
}

sealed trait ScrollServerBase extends Product {
  def scrollUrl(scrollId: String): String

  def baseUrl(scrollId: String): String = s"${scrollUrl(scrollId)}paths/"
  def segmentUrl(segment: SegmentReference): String =
    s"${baseUrl(segment.scrollId)}${segment.segmentId}/"

  def maskFor(segment: SegmentReference): String =
    s"${segmentUrl(segment)}/${segment.segmentId}_mask.png"

  def inklabelFor(segment: SegmentReference): String =
    s"${segmentUrl(segment)}/${segment.segmentId}_inklabels.png"

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

case object FragmentsBase extends ScrollServerBase {
  def scrollUrl(scroll: String): String =
    s"http://dl.ash2txt.org/fragments/$scroll.volpkg/"
}

trait FragmentLikeBase extends ScrollServerBase {
  def scrollUrl(scroll: String): String =
    s"http://dl.ash2txt.org/fragments/$scroll.volpkg/"

  override def baseUrl(scrollId: String): String =
    s"${scrollUrl(scrollId)}working/"

  override def layerUrl(segment: SegmentReference, z: Int): String =
    if (segment.scrollId == "Frag4") f"${segmentUrl(segment)}PHercParis1Fr39_54keV_surface_volume/$z%02d.tif"
    else f"${segmentUrl(segment)}surface_volume/$z%02d.tif"

  override def maskFor(segment: SegmentReference): String =
    if (segment.scrollId == "Frag4") f"${segmentUrl(segment)}PHercParis1Fr39_54keV_mask.png"
    else s"${segmentUrl(segment)}mask.png"

  override def inklabelFor(segment: SegmentReference): String =
    if (segment.scrollId == "Frag4") f"${segmentUrl(segment)}PHercParis1Fr39_54keV_inklabels.png"
    else s"${segmentUrl(segment)}inklabels.png"
}
case object OldFragmentsBase extends FragmentLikeBase
case object PHerc1667Cr01Fr03FragmentBase extends FragmentLikeBase {
  override def segmentUrl(segment: SegmentReference): String =
    s"${super.segmentUrl(segment)}registered/"
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