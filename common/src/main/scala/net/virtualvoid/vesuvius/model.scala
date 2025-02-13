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

  def predictionUrl: String = base.predictionUrlFor(this)

  def areaUrl: String = s"${baseUrl}area_cm2.txt"
  def authorUrl: String = s"${baseUrl}author.txt"

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
  def scrollNumber = newScrollId.number
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
  def predictionUrlFor(segment: SegmentReference): String
  def layerUrl(segment: SegmentReference, z: Int): String
  def layerFileExtension: String
  def segmentIdForDirectory(dirName: String): String
  def isValidSegmentDirectory(dirName: String): Boolean
  def isHighResSegment(segment: SegmentReference): Boolean

  def shortStyleName: String

  def defaultVolumeId(segmentReference: SegmentReference): String = segmentReference.scrollRef.defaultVolumeId
}

sealed trait RegularSegmentDirectoryStyle extends SegmentDirectoryStyle {
  def baseUrl(scrollRef: ScrollReference): String = s"${scrollRef.scrollUrl}paths/"
  def segmentUrl(segment: SegmentReference): String = s"${baseUrl(segment.scrollRef)}${segmentIdInFileName(segment)}/"
  def maskFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segmentIdInFileName(segment)}_mask.png"
  def inklabelFor(segment: SegmentReference): String = s"${segmentUrl(segment)}inklabels.png"
  def objFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segmentIdInFileName(segment)}.obj"
  def ppmFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segmentIdInFileName(segment)}.ppm"
  def compositeFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segmentIdInFileName(segment)}.tif"
  def metaFor(segment: SegmentReference): String = s"${segmentUrl(segment)}meta.json"
  def predictionUrlFor(segment: SegmentReference): String = s"${segmentUrl(segment)}prediction.png"
  def layerUrl(segment: SegmentReference, z: Int): String =
    if (isHighResSegment(segment))
      f"${segmentUrl(segment)}layers/$z%03d.$layerFileExtension"
    else
      f"${segmentUrl(segment)}layers/$z%02d.$layerFileExtension"

  def layerFileExtension: String = "tif"
  def segmentIdForDirectory(dirName: String): String = dirName
  def isValidSegmentDirectory(dirName: String): Boolean = true

  def segmentIdInFileName(segment: SegmentReference): String = segment.segmentId
}
case object RegularSegmentDirectoryStyle extends RegularSegmentDirectoryStyle {
  def isHighResSegment(segment: SegmentReference): Boolean =
    (segment.scrollId == "1667" && segment.segmentId < "20231210132040") ||
      (segment.scrollId == "0332" && segment.segmentId < "20240618142020") ||
      (segment.scrollId == "Frag2")

  override def isValidSegmentDirectory(dirName: String): Boolean =
    !dirName.endsWith("_superseded")

  def shortStyleName: String = "regular"
}
case class RegularSegmentDirectoryStyleAtDifferentBase(scrollBaseUrl: ScrollReference => String, highRes: Boolean = false, val shortStyleName: String) extends RegularSegmentDirectoryStyle {
  override def baseUrl(scrollRef: ScrollReference): String = scrollBaseUrl(scrollRef)
  override def isHighResSegment(segment: SegmentReference): Boolean = highRes
}

case class AutoSegmentedDirectoryStyle(scrollId: String, runName: String, basePath: String) extends SegmentDirectoryStyle {
  def baseUrl(scrollRef: ScrollReference): String = {
    if (scrollRef.scrollId == scrollId) s"${scrollRef.scrollUrl}$basePath/"
    else s"${scrollRef.scrollUrl}thaumato_segments_need_to_be_setup_manually/"
  }

  def segmentIdWithoutRunNamePrefix(segment: SegmentReference): String = {
    require(segment.segmentId.startsWith(s"${runName}_"))
    segment.segmentId.drop(runName.length + 1)
  }

  def segmentUrl(segment: SegmentReference): String = s"${baseUrl(segment.scrollRef)}working_${segmentIdWithoutRunNamePrefix(segment)}/"

  def shortSegmentId(segment: SegmentReference): String =
    if (segment.scrollId == "1" && segment.segmentId.endsWith("_1"))
      segmentIdWithoutRunNamePrefix(segment).dropRight(2)
    else
      segmentIdWithoutRunNamePrefix(segment)

  def maskFor(segment: SegmentReference): String =  s"${segmentUrl(segment)}${shortSegmentId(segment)}_mask.png"

  def inklabelFor(segment: SegmentReference): String = s"${segmentUrl(segment)}inklabels.png"
  def objFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${shortSegmentId(segment)}.obj"
  def ppmFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${shortSegmentId(segment)}.ppm"
  def compositeFor(segment: SegmentReference): String = s"${segmentUrl(segment)}composite.$layerFileExtension"
  def metaFor(segment: SegmentReference): String = s"${segmentUrl(segment)}meta.json"
  def layerFileExtension: String = "jpg"
  def layerUrl(segment: SegmentReference, z: Int): String = f"${segmentUrl(segment)}layers/$z%02d.$layerFileExtension"

  def segmentIdForDirectory(dirName: String): String = {
    require(dirName.startsWith("working_"))
    s"${runName}_${dirName.drop("working_".length)}"
  }
  def isValidSegmentDirectory(dirName: String): Boolean = dirName.startsWith("working_")

  def predictionUrlFor(segment: SegmentReference): String = {
    val prefix =
      if (segment.scrollId == "1") ""
      else "../"

    s"${baseUrl(segment.scrollRef)}${prefix}predictions/working_${segmentIdWithoutRunNamePrefix(segment)}_prediction_rotated_0_layer_17.png"
  }

  def isHighResSegment(segment: SegmentReference): Boolean = false

  def shortStyleName: String = s"thaumato-$runName"
}

case object BrunissAutogens extends RegularSegmentDirectoryStyle {
  override def baseUrl(scrollRef: ScrollReference): String = {
    val dirName = "autogens"
    s"https://dl.ash2txt.org/community-uploads/bruniss/scrolls/s${scrollRef.scrollNumber}/$dirName/"
  }

  // do not try to parse fasp meta.json
  override def metaFor(segment: SegmentReference): String = s"${segmentUrl(segment)}meta-info.json"
  override def predictionUrlFor(segment: SegmentReference): String = s"${segmentUrl(segment)}prediction.png"

  override def layerFileExtension: String = "tif"
  override def layerUrl(segment: SegmentReference, z: Int): String =
    f"${segmentUrl(segment)}layers/${z - 22}%02d.$layerFileExtension"

  override def segmentIdForDirectory(dirName: String): String = s"autogens-$dirName"
  override def segmentIdInFileName(segment: SegmentReference): String = {
    require(segment.segmentId.startsWith("autogens-"))
    segment.segmentId.drop("autogens-".length)
  }

  def isHighResSegment(segment: SegmentReference): Boolean = false

  override def defaultVolumeId(segmentReference: SegmentReference): String =
    if (segmentReference.newScrollId.number == 3) "20231117143551" // 7.91um volume
    else super.defaultVolumeId(segmentReference)

  def shortStyleName: String = "bruniss-autogens"
}

case object WaldkauzFaspDirectoryStyle extends RegularSegmentDirectoryStyle {
  override def baseUrl(scrollRef: ScrollReference): String =
    if (scrollRef.newScrollId.number == 1)
      "https://dl.ash2txt.org/community-uploads/waldkauz/fasp/"
    else
      "https://dl.ash2txt.org/community-uploads/waldkauz/fasp/missing"

  override def segmentUrl(segment: SegmentReference): String = segment.segmentId match {
    case "waldkauz-fasp-v1" => s"${baseUrl(segment.scrollRef)}v1/"
    case "waldkauz-fasp-v3" => s"${baseUrl(segment.scrollRef)}v3/"
    case "waldkauz-fasp-v4" => s"${baseUrl(segment.scrollRef)}v4/"
    case _                  => throw new IllegalArgumentException(s"Unknown waldkauz segment ${segment.segmentId}")
  }

  override def layerFileExtension: String = "tif"
  override def layerUrl(segment: SegmentReference, z: Int): String =
    segment.segmentId match {
      case "waldkauz-fasp-v1" => f"${segmentUrl(segment)}fullres/${z - 22}%02d.$layerFileExtension"
      case "waldkauz-fasp-v3" => f"${segmentUrl(segment)}layers_hr/${z - 22}%02d.$layerFileExtension"
      case "waldkauz-fasp-v4" => f"${segmentUrl(segment)}layers_hr/${z - 22}%02d.$layerFileExtension"
      case _                  => throw new IllegalArgumentException(s"Unknown waldkauz segment ${segment.segmentId}")
    }

  override def objFor(segment: SegmentReference): String = segment.segmentId match {
    case "waldkauz-fasp-v1" => "https://dl.ash2txt.org/community-uploads/jrudolph/tmp/fasp-new.obj"
    case "waldkauz-fasp-v3" => "https://dl.ash2txt.org/community-uploads/jrudolph/tmp/fasp-v3-new.obj"
    // did not change for v4
    case "waldkauz-fasp-v4" => "https://dl.ash2txt.org/community-uploads/jrudolph/tmp/fasp-v3-new.obj"
    case _                  => throw new IllegalArgumentException(s"Unknown waldkauz segment ${segment.segmentId}")
  }

  override def segmentIdForDirectory(dirName: String): String = dirName match {
    case "v1" => "waldkauz-fasp-v1"
    case "v3" => "waldkauz-fasp-v3"
    case "v4" => "waldkauz-fasp-v4"
    case _ =>
      throw new IllegalArgumentException(s"Unknown waldkauz directory $dirName")
  }

  override def isValidSegmentDirectory(dirName: String): Boolean =
    dirName match {
      case "v1" | "v3" | "v4" => true
      case _ => false
    }

  def isHighResSegment(segment: SegmentReference): Boolean = false

  def shortStyleName: String = "waldkauz-fasp"
}

case object FlatSegmentedDirectoryStyle extends RegularSegmentDirectoryStyle {
  override def isHighResSegment(segment: SegmentReference): Boolean = false
  override def maskFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}_flat_mask.png"
  override def objFor(segment: SegmentReference): String = s"${segmentUrl(segment)}${segment.segmentId}_flat.obj"
  override def layerFileExtension: String = "jpg"

  def shortStyleName: String = "flat-segmented"
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

  def predictionUrlFor(segment: SegmentReference): String =
    directoryStyleFor(segment).predictionUrlFor(segment)

  def isHighResSegment(segment: SegmentReference): Boolean =
    directoryStyleFor(segment).isHighResSegment(segment)

  def directoryStyleFor(segment: SegmentReference): SegmentDirectoryStyle

  /** The default volume to choose if no explicit volume is provided */
  def defaultVolumeFor(segment: SegmentReference): String =
    directoryStyleFor(segment).defaultVolumeId(segment)

  def supportedDirectoryStyles: Seq[SegmentDirectoryStyle]

  def isFragment: Boolean
}

val StephaneSegmentStyle =
  RegularSegmentDirectoryStyleAtDifferentBase(
    scrollRef => s"https://dl.ash2txt.org/community-uploads/stephane/Scroll${scrollRef.newScrollId.number}.${scrollRef.newScrollId.name}/",
    shortStyleName = "stephane-segments"
  )

val StephaneSegmentIds = Set(
  "20241202095341",
  "20241202123309",
  "20241202142344",
  "20241202151506",
  "20241202164924",
  "20241204143830",
  "20241206093955",
  "20241206170633",
  "20241206190644",
  "20241207125748",
)

val ThaumatoRuns = Seq(
  AutoSegmentedDirectoryStyle("1", "thaumato_20240821000000", "thaumato_outputs/scroll1_autosegmentation_20240821000000"),
  AutoSegmentedDirectoryStyle("1", "thaumato_20241003234631", "thaumato_outputs/scroll1_autosegmentation_20241003234631/working"),
  AutoSegmentedDirectoryStyle("172", "thaumato_2024_nov1", "thaumato_outputs/scroll5_thaumato_nov1/working"),
  AutoSegmentedDirectoryStyle("172", "thaumato_2025_jan15", "thaumato_outputs/scroll5_thaumato_jan15/working"),
)

case object FullScrollsBase extends ScrollServerBase {

  def scrollUrl(newScrollId: NewScrollId): String =
    s"https://dl.ash2txt.org/full-scrolls/Scroll${newScrollId.number}/${newScrollId.name}.volpkg/"

  def directoryStyleFor(segment: SegmentReference): SegmentDirectoryStyle =
    if (segment.segmentId.startsWith("thaumato_"))
      ThaumatoRuns.find(t => segment.segmentId.startsWith(t.runName)).get
    else if (segment.segmentId .startsWith("autogens-"))
      BrunissAutogens
    else if (StephaneSegmentIds(segment.segmentId))
      StephaneSegmentStyle
    else if (segment.scrollRef.newScrollId.number == 5)
      FlatSegmentedDirectoryStyle
    else if (segment.segmentId .startsWith("waldkauz-fasp"))
      WaldkauzFaspDirectoryStyle
    else
      RegularSegmentDirectoryStyle

  val supportedDirectoryStyles: Seq[SegmentDirectoryStyle] =
    Seq(
      RegularSegmentDirectoryStyle,
      StephaneSegmentStyle,
      BrunissAutogens,
      WaldkauzFaspDirectoryStyle) ++ ThaumatoRuns

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

  def shortStyleName: String = "fragment"
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

sealed trait SegmentArtifact {
  def urlFor(segment: SegmentReference): String
  def fileNameFor(segment: SegmentReference): String
}
object SegmentArtifact {
  val Mask = apply(_.maskUrl, segment => s"${segment.segmentId}_mask.${extOf(segment.maskUrl)}")
  val Composite = apply(_.compositeUrl, segment => s"${segment.segmentId}_composite.${extOf(segment.compositeUrl)}")
  val Inklabel = apply(_.inklabelUrl, segment => s"${segment.segmentId}_inklabel.${extOf(segment.inklabelUrl)}")
  val PPM = apply(_.ppmUrl, segment => s"${segment.segmentId}.ppm")
  val Obj = apply(_.objUrl, segment => s"${segment.segmentId}.obj")
  val Meta = apply(_.metaUrl, segment => s"${segment.segmentId}-meta.json")
  val Author = apply(_.authorUrl, segment => s"${segment.segmentId}-author.txt")
  val Area = apply(_.areaUrl, segment => s"${segment.segmentId}-areaCm2.txt")
  val Layer32 = apply(_.layerUrl(32), segment => s"${segment.segmentId}-layer32.${extOf(segment.layerUrl(32))}")

  def extOf(name: String): String =
    name.split("\\.").last

  private def apply(get: SegmentReference => String, fileName: SegmentReference => String): SegmentArtifact = new SegmentArtifact {
    def urlFor(segment: SegmentReference): String = get(segment)
    def fileNameFor(segment: SegmentReference): String = fileName(segment)
  }
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

case class SegmentInfo(
    ref:            SegmentReference,
    width:          Int,
    height:         Int,
    area:           Option[Float],
    metadata:       Option[SegmentMetadata],
    minZ:           Option[Int],
    maxZ:           Option[Int],
    volumeMetadata: Option[VolumeMetadata],
    author:         Option[String]
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
