package net.virtualvoid.vesuvius

import spray.json._

sealed trait WorkItem {
  def id: String
}
object WorkItem {
  import DefaultJsonProtocol._

  implicit val inferenceWorkItemFormat: JsonFormat[InferenceWorkItem] = jsonFormat6(InferenceWorkItem.apply)
  implicit val workItemFormat: RootJsonFormat[WorkItem] = new RootJsonFormat[WorkItem] {
    // FIXME: other potential item types
    override def write(obj: WorkItem): JsValue = obj match {
      case i: InferenceWorkItem => inferenceWorkItemFormat.write(i)
    }
    override def read(json: JsValue): WorkItem =
      inferenceWorkItemFormat.read(json)
  }
}
case class InferenceWorkItem(
    id:            String,
    segment:       SegmentReference,
    model:         String,
    startLayer:    Int,
    stride:        Int,
    reverseLayers: Boolean
) extends WorkItem

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