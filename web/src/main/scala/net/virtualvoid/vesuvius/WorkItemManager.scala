package net.virtualvoid.vesuvius

import org.apache.pekko.http.scaladsl.model.DateTime

import scala.collection.immutable.ListMap
import scala.util.Random

trait WorkItemManager {
  def assignNext(workerId: String, allowedTypes: String => Boolean): Option[WorkItem]
  def findItem(id: String): Option[WorkItem]
  def markDone(workerId: String, workItem: WorkItem): Unit

  def itemStates: Map[WorkItem, WorkItemManager.ItemState]
}

object WorkItemManager {
  sealed trait ItemState
  case object Queued extends ItemState
  case class Assigned(workerId: String, atMillis: Long) extends ItemState {
    def at: DateTime = DateTime(atMillis)
    def runningForSeconds: Int = ((System.currentTimeMillis() - atMillis) / 1000).toInt
  }

  def apply(initialItems: Seq[WorkItem], segmentIds: Seq[SegmentReference]): WorkItemManager = {
    var itemState = ListMap[WorkItem, ItemState](initialItems.map(_ -> Queued)*)
    val targetList = segmentIds.map(_.segmentId).distinct.toVector.sorted

    new WorkItemManager {
      def assignNext(workerId: String, allowedTypes: String => Boolean): Option[WorkItem] =
        synchronized {
          val possible = itemState.filter(x => allowedTypes(x._1.input.productPrefix) && x._2 == Queued)
          val workerRandom = new Random(workerId.hashCode)
          val workerPerm = workerRandom.shuffle(targetList)
          val firstPossible = workerPerm.find(id => possible.exists(_._1.segment.segmentId == id))
          firstPossible.flatMap(id => possible.find(_._1.segment.segmentId == id)).map {
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

      def itemStates: Map[WorkItem, ItemState] = itemState
    }
  }
}