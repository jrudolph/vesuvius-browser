package net.virtualvoid.vesuvius

case class SegmentReference(scrollRef: ScrollReference, segmentId: String) {
  def scrollId: String = scrollRef.scrollId

  def newScrollId: NewScrollId = scrollRef.newScrollId

  def base: ScrollServerBase = scrollRef.base
  def baseUrl: String = base.segmentUrl(this)

  def layerUrl(z: Int): String = base.layerUrl(this, z)
  def layerFileExtension: String = base.layerFileExtension(this)

  def maskUrl: String = base.maskFor(this)
  def objUrl: String = base.objUrlFor(this)
  def ppmUrl: String = base.ppmUrlFor(this)
  def compositeUrl: String = base.compositeUrlFor(this)
  def inklabelUrl: String = base.inklabelFor(this)

  def metaUrl: String = base.metaFor(this)

  def isHighResSegment: Boolean = base.isHighResSegment(this)
}
object SegmentReference {
  import spray.json._
  import DefaultJsonProtocol._

  implicit val scrollServerBaseFormat: RootJsonFormat[ScrollServerBase] =
    new RootJsonFormat[ScrollServerBase] {
      def write(obj: ScrollServerBase): JsValue = JsString(obj.productPrefix)
      def read(json: JsValue): ScrollServerBase = json.convertTo[String] match {
        case "FullScrollsBase" => FullScrollsBase
        case "FragmentsBase"   => FragmentsBase
      }
    }
  implicit val newScrollIdFormat: RootJsonFormat[NewScrollId] = jsonFormat2(NewScrollId.apply)
  implicit val scrollReferenceFormat: RootJsonFormat[ScrollReference] = jsonFormat4(ScrollReference.apply)
  implicit val segmentReferenceFormat: RootJsonFormat[SegmentReference] = jsonFormat2(SegmentReference.apply)
}

case class NewScrollId(number: Int, name: String)

case class ScrollReference(scrollId: String, newScrollId: NewScrollId, base: ScrollServerBase, defaultVolumeId: String) {
  def scrollUrl: String = base.scrollUrl(newScrollId)
  def volumeMetadataUrl(volumeId: String): String = s"${volumeUrl(volumeId)}meta.json"
  def volumeUrl(volumeId: String): String = s"${scrollUrl}volumes/$volumeId/"
  def volumeGridUrl(volumeId: String): String = s"${scrollUrl}volume_grids/$volumeId/"

  def isFragment: Boolean = base.isFragment
}

object ScrollReference {
  val scrolls: Seq[ScrollReference] = Seq(
    ScrollReference("1", 1, "PHercParis4", FullScrollsBase, "20230205180739"),
    ScrollReference("2", 2, "PHercParis3", FullScrollsBase, "20230210143520"),
    ScrollReference("0332", 3, "PHerc332", FullScrollsBase, "20231027191953"),
    ScrollReference("1667", 4, "PHerc1667", FullScrollsBase, "20231107190228"),
    ScrollReference("172", 5, "PHerc172", FullScrollsBase, "20241024131838"),

    ScrollReference("Frag1", 1, "PHercParis2Fr47", FragmentsBase, "20230205142449"), // 2nd volume: 20230213100222
    ScrollReference("Frag2", 2, "PHercParis2Fr143", FragmentsBase, "20230216174557"), // 2nd volume: 20230226143835
    ScrollReference("Frag3", 3, "PHercParis1Fr34", FragmentsBase, "20230212182547"), // 2nd volume: 20230215142309
    ScrollReference("Frag4", 4, "PHercParis1Fr39", FragmentsBase, "20230215185642"), // 2nd volume: 20230222173037
    ScrollReference("PHerc1667Cr01Fr03", 5, "PHerc1667Cr1Fr3", FragmentsBase, "20231121133215"),
    ScrollReference("PHerc0051Cr04Fr08", 6, "PHerc51Cr4Fr8", FragmentsBase, "20231121152933"),
  )

  private def apply(oldScrollId: String, scrollNumber: Int, scrollName: String, base: ScrollServerBase, defaultVolumeId: String): ScrollReference =
    ScrollReference(oldScrollId, NewScrollId(scrollNumber, scrollName), base, defaultVolumeId)

  def byId(id: Int): Option[ScrollReference] =
    scrolls.find(_.scrollId == id.toString)

  def byId(id: String): Option[ScrollReference] = scrolls.find(_.scrollId == id)
}

sealed trait SegmentDirectoryStyle extends Product {
  def baseUrl(scrollRef: ScrollReference): String
  def segmentUrl(segment: SegmentReference): String
  def maskFor(segment: SegmentReference): String
  def inklabelFor(segment: SegmentReference): String
  def objFor(segment: SegmentReference): String
  def ppmFor(segment: SegmentReference): String
  def compositeFor(segment: SegmentReference): String
  def metaFor(segment: SegmentReference): String
  def layerUrl(segment: SegmentReference, z: Int): String
  def layerFileExtension: String
  def segmentIdForDirectory(dirName: String): String
  def isValidSegmentDirectory(dirName: String): Boolean
  def isHighResSegment(segment: SegmentReference): Boolean
}

sealed trait RegularSegmentDirectoryStyle extends SegmentDirectoryStyle {
  def baseUrl(scrollRef: ScrollReference): String = s"${scrollRef.scrollUrl}paths/"
  def segmentUrl(segment: SegmentReference): String = s"${baseUrl(segment.scrollRef)}${segment.segmentId}/"
  def maskFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}_mask.png"
  def inklabelFor(segment: SegmentReference): String = s"${segmentUrl(segment)}inklabels.png"
  def objFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}.obj"
  def ppmFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}.ppm"
  def compositeFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}.tif"
  def metaFor(segment: SegmentReference): String = s"${segmentUrl(segment)}meta.json"
  def layerUrl(segment: SegmentReference, z: Int): String =
    if (isHighResSegment(segment))
      f"${segmentUrl(segment)}layers/$z%03d.$layerFileExtension"
    else
      f"${segmentUrl(segment)}layers/$z%02d.$layerFileExtension"

  def layerFileExtension: String = "tif"
  def segmentIdForDirectory(dirName: String): String = dirName
  def isValidSegmentDirectory(dirName: String): Boolean = true
}
case object RegularSegmentDirectoryStyle extends RegularSegmentDirectoryStyle {
  def isHighResSegment(segment: SegmentReference): Boolean =
    (segment.scrollId == "1667" && segment.segmentId < "20231210132040") ||
      (segment.scrollId == "0332" && segment.segmentId < "20240618142020") ||
      (segment.scrollId == "Frag2")
}
case object AutoSegmentedDirectoryStyle extends SegmentDirectoryStyle {
  def baseUrl(scrollRef: ScrollReference): String =
    if (scrollRef.scrollId == "1") s"${scrollRef.scrollUrl}scroll1_autosegmentation_20240821000000/"
    else if (scrollRef.scrollId == "172") s"${scrollRef.scrollUrl}thaumato_outputs/scroll5_thaumato_nov1/working/"
    else s"${scrollRef.scrollUrl}thaumato_outputs"
  def segmentUrl(segment: SegmentReference): String = s"${baseUrl(segment.scrollRef)}working_${segment.segmentId}/"

  def shortSegmentId(segment: SegmentReference): String =
    if (segment.scrollId == "1" && segment.segmentId.endsWith("_1"))
      segment.segmentId.dropRight(2)
    else
      segment.segmentId

  def maskFor(segment: SegmentReference): String = {
    s"${segmentUrl(segment)}${shortSegmentId(segment)}_mask.png"
  }

  def inklabelFor(segment: SegmentReference): String = s"${segmentUrl(segment)}inklabels.png"
  def objFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${shortSegmentId(segment)}.obj"
  def ppmFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${shortSegmentId(segment)}.ppm"
  def compositeFor(segment: SegmentReference): String = s"${segmentUrl(segment)}composite.png"
  def metaFor(segment: SegmentReference): String = s"${segmentUrl(segment)}meta.json"
  def layerFileExtension: String = "jpg"
  def layerUrl(segment: SegmentReference, z: Int): String = f"${segmentUrl(segment)}layers/$z%02d.$layerFileExtension"

  def segmentIdForDirectory(dirName: String): String = {
    require(dirName.startsWith("working_"))
    dirName.drop("working_".length)
  }
  def isValidSegmentDirectory(dirName: String): Boolean = dirName.startsWith("working_")

  def predictionUrlFor(segment: SegmentReference): String = {
    val prefix =
      if (segment.scrollId == "1") ""
      else "../"

    s"${baseUrl(segment.scrollRef)}${prefix}predictions/working_${segment.segmentId}_prediction_rotated_0_layer_17.png"
  }

  def isHighResSegment(segment: SegmentReference): Boolean = false
}
case object FlatSegmentedDirectoryStyle extends RegularSegmentDirectoryStyle {
  override def isHighResSegment(segment: SegmentReference): Boolean = false
  override def maskFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}_flat_mask.png"
  override def objFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}_flat.obj"
  override def layerFileExtension: String = "jpg"
}

sealed trait ScrollServerBase extends Product {
  def scrollUrl(newScrollId: NewScrollId): String

  /* def baseUrl(newScrollId: NewScrollId): String =
    s"${scrollUrl(newScrollId)}paths/" */
  def segmentUrl(segment: SegmentReference): String =
    directoryStyleFor(segment).segmentUrl(segment)

  def maskFor(segment: SegmentReference): String =
    directoryStyleFor(segment).maskFor(segment)

  def inklabelFor(segment: SegmentReference): String =
    directoryStyleFor(segment).inklabelFor(segment)

  def objUrlFor(segment: SegmentReference): String =
    directoryStyleFor(segment).objFor(segment)

  def ppmUrlFor(segment: SegmentReference): String =
    directoryStyleFor(segment).ppmFor(segment)

  def compositeUrlFor(segment: SegmentReference): String =
    directoryStyleFor(segment).compositeFor(segment)

  def metaFor(segment: SegmentReference): String =
    directoryStyleFor(segment).metaFor(segment)

  def layerUrl(segment: SegmentReference, z: Int): String =
    directoryStyleFor(segment).layerUrl(segment, z)

  def layerFileExtension(segment: SegmentReference): String =
    directoryStyleFor(segment).layerFileExtension

  def isHighResSegment(segment: SegmentReference): Boolean =
    directoryStyleFor(segment).isHighResSegment(segment)

  def directoryStyleFor(segment: SegmentReference): SegmentDirectoryStyle

  def supportedDirectoryStyles: Seq[SegmentDirectoryStyle]

  def isFragment: Boolean
}

case object FullScrollsBase extends ScrollServerBase {

  def scrollUrl(newScrollId: NewScrollId): String =
    s"https://dl.ash2txt.org/full-scrolls/Scroll${newScrollId.number}/${newScrollId.name}.volpkg/"

  def directoryStyleFor(segment: SegmentReference): SegmentDirectoryStyle =
    if (segment.segmentId.startsWith("mesh_"))
      AutoSegmentedDirectoryStyle
    else if (segment.scrollRef.newScrollId.number == 5)
      FlatSegmentedDirectoryStyle
    else
      RegularSegmentDirectoryStyle

  val supportedDirectoryStyles: Seq[SegmentDirectoryStyle] = Seq(RegularSegmentDirectoryStyle, AutoSegmentedDirectoryStyle)

  def isFragment = false
}

sealed trait FragmentDirectoryStyle extends RegularSegmentDirectoryStyle {
  override def baseUrl(scrollRef: ScrollReference): String =
    s"${scrollRef.scrollUrl}working/"

  def isHighResSegment(segment: SegmentReference): Boolean = false
}
case object FragmentDirectoryStyle extends FragmentDirectoryStyle {
  override def segmentUrl(segment: SegmentReference): String = segment.scrollId match {
    case "PHerc1667Cr01Fr03" | "PHerc0051Cr04Fr08" => f"${super.segmentUrl(segment)}surface_processing/"
    case _                                         => super.segmentUrl(segment)
  }

  override def layerUrl(segment: SegmentReference, z: Int): String =
    if (segment.scrollId == "Frag4")
      f"${segmentUrl(segment)}PHercParis1Fr39_54keV_surface_volume/$z%02d.tif"
    else
      f"${segmentUrl(segment)}surface_volume/$z%02d.tif"

  override def maskFor(segment: SegmentReference): String =
    segment.scrollId match {
      case "Frag4" => f"${segmentUrl(segment)}PHercParis1Fr39_54keV_mask.png"
      case _       => s"${segmentUrl(segment)}mask.png"
    }

  override def objFor(segment: SegmentReference): String =
    segment.scrollId match {
      case "Frag4" => f"${segmentUrl(segment)}extras/PHercParis1Fr39_54keV.obj"
      case _       => s"${segmentUrl(segment)}result.obj"
    }

  override def inklabelFor(segment: SegmentReference): String =
    if (segment.scrollId == "Frag4")
      f"${segmentUrl(segment)}PHercParis1Fr39_54keV_inklabels.png"
    else
      super.inklabelFor(segment)

}

case object FragmentsBase extends ScrollServerBase {
  def scrollUrl(newScrollId: NewScrollId): String =
    s"https://dl.ash2txt.org/fragments/Frag${newScrollId.number}/${newScrollId.name}.volpkg/"

  override def isHighResSegment(segment: SegmentReference): Boolean = false

  def directoryStyleFor(segment: SegmentReference): SegmentDirectoryStyle =
    if (segment.segmentId.startsWith("202"))
      RegularSegmentDirectoryStyle
    else
      FragmentDirectoryStyle

  val supportedDirectoryStyles: Seq[SegmentDirectoryStyle] = Seq(FragmentDirectoryStyle, RegularSegmentDirectoryStyle)

  def isFragment = true
}

case class SegmentMetadata(
    name:   String,
    uuid:   String,
    volume: String
)
object SegmentMetadata {
  import spray.json._
  import DefaultJsonProtocol._

  implicit val segmentMetadataFormat: RootJsonFormat[SegmentMetadata] = jsonFormat3(SegmentMetadata.apply)
}

case class ImageInfo(
    ref:            SegmentReference,
    width:          Int,
    height:         Int,
    area:           Option[Float],
    metadata:       Option[SegmentMetadata],
    volumeMetadata: Option[VolumeMetadata]
) {
  def scrollId: String = ref.scrollId
  def segmentId: String = ref.segmentId

  def isLandscape: Boolean = width > height
}

case class InferenceModelArchitecture(
    name: String,
    url:  String
)
object InferenceModelArchitecture {
  val GrandPrizeModel = InferenceModelArchitecture(
    "Grand Prize Model",
    "https://github.com/younader/Vesuvius-Grandprize-Winner"
  )
  val FirstWordModel = InferenceModelArchitecture(
    "First Word Model",
    "https://github.com/younader/Vesuvius-First-Letters"
  )

  val Architectures: Seq[InferenceModelArchitecture] = Seq(
    GrandPrizeModel,
    FirstWordModel
  )

  import spray.json._
  import DefaultJsonProtocol._
  implicit val inferenceModelArchitectureFormat: RootJsonFormat[InferenceModelArchitecture] = jsonFormat2(InferenceModelArchitecture.apply)
}

case class InferenceModelCheckpoint(
    name:          String,
    shortName:     String,
    architecture:  InferenceModelArchitecture,
    checkpointUrl: String
)
object InferenceModelCheckpoint {

  val GrandPrizeModel = InferenceModelCheckpoint(
    "2023 Grand Prize Model",
    "grand-prize",
    InferenceModelArchitecture.GrandPrizeModel,
    "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_wild15_20230702185753_0_fr_i3depoch=12.ckpt"
  )
  val GrandPrizeFineTune0 = InferenceModelCheckpoint(
    "Grand Prize JR Fine-Tune 0",
    "grand-prize-finetune0",
    InferenceModelArchitecture.GrandPrizeModel,
    "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_4972_epoch_3_by_valid_loss_0.704.ckpt"
  )
  val GrandPrizeFineTune1 = InferenceModelCheckpoint(
    "Grand Prize JR Fine-Tune 1",
    "grand-prize-finetune1",
    InferenceModelArchitecture.GrandPrizeModel,
    "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_9800_by_train_loss_0.575.ckpt"
  )
  val GrandPrizeFineTune2 = InferenceModelCheckpoint(
    "Grand Prize JR Fine-Tune 2",
    "grand-prize-finetune2",
    InferenceModelArchitecture.GrandPrizeModel,
    "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_8701_epoch_6_by_valid_loss_0.702.ckpt"
  )
  val GrandPrizeFineTune3 = InferenceModelCheckpoint(
    "Grand Prize JR Fine-Tune 3",
    "grand-prize-finetune3",
    InferenceModelArchitecture.GrandPrizeModel,
    "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_14900_by_train_loss_0.556.ckpt"
  )

  val TimesformerScroll5_27112024 = InferenceModelCheckpoint(
    "Timesformer Model trained on Scroll 5 / 172 data (timesformer_scroll5_27112024)",
    "timesformer-scroll5-27112024",
    InferenceModelArchitecture.GrandPrizeModel,
    "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_scroll5_27112024_20241108111522_epoch=19.ckpt"
  )

  val FirstWordModel = InferenceModelCheckpoint(
    "First Word Model",
    "first-word",
    InferenceModelArchitecture.FirstWordModel,
    "https://f004.backblazeb2.com/file/bulk-data-jr/model.ckpt"
  )

  val models: Seq[InferenceModelCheckpoint] = Seq(
    GrandPrizeModel,
    GrandPrizeFineTune0,
    GrandPrizeFineTune1,
    GrandPrizeFineTune2,
    GrandPrizeFineTune3,
    TimesformerScroll5_27112024,
    FirstWordModel
  )

  import spray.json._
  import DefaultJsonProtocol._
  implicit val inferenceModelCheckpointFormat: RootJsonFormat[InferenceModelCheckpoint] = jsonFormat4(InferenceModelCheckpoint.apply)
}
