package net.virtualvoid.vesuvius

import spray.json._

case class WorkItem(
    id:      String,
    segment: SegmentReference,
    `type`:  String,
    input:   WorkItemInput
)
object WorkItem {
  import DefaultJsonProtocol._
  implicit val workItemFormat: RootJsonFormat[WorkItem] = jsonFormat4(WorkItem.apply)
}

sealed trait WorkItemInput extends Product {
  def `type`: String
}
object WorkItemInput extends SprayJsonHelpers {
  import DefaultJsonProtocol._

  implicit val inferenceParametersFormat: JsonFormat[InferenceParameters] = jsonFormat3(InferenceParameters.apply)
  implicit val inferenceWorkItemFormat: JsonFormat[InferenceWorkItemInput] = jsonFormat3(InferenceWorkItemInput.apply)
  implicit val ppmFingerprintWorkItemFormat: JsonFormat[PPMFingerprintWorkItemInput.type] = jsonFormat0(() => PPMFingerprintWorkItemInput)
  implicit val downsamplePPMWorkItemFormat: JsonFormat[DownsamplePPMWorkItemInput] = jsonFormat2(DownsamplePPMWorkItemInput.apply)
  implicit val workItemFormat: RootJsonFormat[WorkItemInput] = new RootJsonFormat[WorkItemInput] {
    override def write(obj: WorkItemInput): JsValue = {
      val res =
        obj match {
          case i: InferenceWorkItemInput     => inferenceWorkItemFormat.write(i)
          case PPMFingerprintWorkItemInput   => ppmFingerprintWorkItemFormat.write(PPMFingerprintWorkItemInput)
          case d: DownsamplePPMWorkItemInput => downsamplePPMWorkItemFormat.write(d)
        }
      res.asJsObject + ("type" -> JsString(obj.productPrefix))
    }
    override def read(json: JsValue): WorkItemInput = json.asJsObject.fields("type") match {
      case JsString("InferenceWorkItemInput")      => inferenceWorkItemFormat.read(json)
      case JsString("PPMFingerprintWorkItemInput") => ppmFingerprintWorkItemFormat.read(json)
      case JsString("DownsamplePPMWorkItemInput")  => downsamplePPMWorkItemFormat.read(json)
      case _                                       => throw new IllegalArgumentException(s"Work item type not specified")
    }
  }
}

case class InferenceParameters(
    startLayer:    Int,
    stride:        Int,
    reverseLayers: Boolean
)

case class InferenceWorkItemInput(
    shortName:       String,
    modelCheckpoint: InferenceModelCheckpoint,
    parameters:      InferenceParameters
) extends WorkItemInput {
  def `type`: String = "inference"
}

case object PPMFingerprintWorkItemInput extends WorkItemInput {
  def `type`: String = "fingerprint"
}

/**
 * A workitem to download and downsample a PPM file.
 *
 * positionType: u16
 * downsamplingBits: u8 How many data items to discard. 0 means no downsampling, 1 means discard every second item, 2 means discard every fourth item, etc.
 */
case class DownsamplePPMWorkItemInput(positionType: String, downsamplingBits: Int) extends WorkItemInput {
  def `type`: String = "downsample_ppm"
}

case class AssetReference()
object AssetReference {
  import DefaultJsonProtocol._
  implicit val assetReferenceFormat: JsonFormat[AssetReference] = jsonFormat0(() => AssetReference())
}

sealed trait WorkItemResult {
  def workItem: WorkItem
}
object WorkItemResult {
  import DefaultJsonProtocol._
  implicit val completeFormat: JsonFormat[WorkCompleted] = jsonFormat2(WorkCompleted.apply)
  implicit val failedFormat: JsonFormat[WorkFailed] = jsonFormat3(WorkFailed.apply)

  implicit val resultFormat: RootJsonFormat[WorkItemResult] = new RootJsonFormat[WorkItemResult] {
    def write(obj: WorkItemResult): JsValue = obj match {
      case c: WorkCompleted => completeFormat.write(c)
      case f: WorkFailed    => failedFormat.write(f)
    }
    def read(json: JsValue): WorkItemResult = {
      if (json.asJsObject.fields.contains("failureMessage"))
        json.convertTo[WorkFailed]
      else
        json.convertTo[WorkCompleted]
    }
  }
}
case class WorkCompleted(
    workItem:      WorkItem,
    resultMessage: String
) extends WorkItemResult
case class WorkFailed(
    workItem:       WorkItem,
    failureMessage: String,
    failureLog:     String
) extends WorkItemResult
