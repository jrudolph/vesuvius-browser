package net.virtualvoid.vesuvius

import scala.collection.immutable.ListMap

trait WorkItemManager {
  def assignNext(workerId: String): Option[WorkItem]
  def findItem(id: String): Option[WorkItem]
  def markDone(workerId: String, workItem: WorkItem): Unit
}

object WorkItemManager {
  def apply(initialItems: Seq[WorkItem]): WorkItemManager = {
    sealed trait ItemState
    case object Queued extends ItemState
    case class Assigned(workerId: String, atMillis: Long) extends ItemState

    var itemState = ListMap[WorkItem, ItemState](initialItems.map(_ -> Queued): _*)

    new WorkItemManager {
      def assignNext(workerId: String): Option[WorkItem] =
        synchronized {
          itemState.find(_._2 == Queued).map {
            case (item, _) =>
              itemState = itemState.updated(item, Assigned(workerId, System.currentTimeMillis()))
              item
          }
        }

      def findItem(id: String): Option[WorkItem] = itemState.keys.find(_.id == id)

      def markDone(workerId: String, workItem: WorkItem): Unit =
        synchronized {
          itemState = itemState.removed(workItem)
        }
    }
  }
}