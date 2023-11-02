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
object WorkItemInput {
  import DefaultJsonProtocol._

  implicit val inferenceWorkItemFormat: JsonFormat[InferenceWorkItemInput] = jsonFormat4(InferenceWorkItemInput.apply)
  implicit val ppmFingerprintWorkItemFormat: JsonFormat[PPMFingerprintWorkItemInput.type] = jsonFormat0(() => PPMFingerprintWorkItemInput)
  implicit val workItemFormat: RootJsonFormat[WorkItemInput] = new RootJsonFormat[WorkItemInput] {
    override def write(obj: WorkItemInput): JsValue = {
      val res =
        obj match {
          case i: InferenceWorkItemInput   => inferenceWorkItemFormat.write(i)
          case PPMFingerprintWorkItemInput => ppmFingerprintWorkItemFormat.write(PPMFingerprintWorkItemInput)
        }
      res.asJsObject + ("type" -> JsString(obj.productPrefix))
    }
    override def read(json: JsValue): WorkItemInput = json.asJsObject.fields("type") match {
      case JsString("InferenceWorkItem")      => inferenceWorkItemFormat.read(json)
      case JsString("PPMFingerprintWorkItem") => ppmFingerprintWorkItemFormat.read(json)
      case _                                  => throw new IllegalArgumentException(s"Work item type not specified")
    }
  }
}
case class InferenceWorkItemInput(
    model:         String,
    startLayer:    Int,
    stride:        Int,
    reverseLayers: Boolean
) extends WorkItemInput {
  def `type`: String = "inference"
}
case object PPMFingerprintWorkItemInput extends WorkItemInput {
  def `type`: String = "fingerprint"
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