package net.virtualvoid.vesuvius

import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.stream.stage.{ GraphStageLogic, GraphStageWithMaterializedValue, OutHandler }
import org.apache.pekko.stream.{ Attributes, Outlet, SourceShape }

import scala.collection.mutable

class PriorityQueueWithRefreshStage[T] extends GraphStageWithMaterializedValue[SourceShape[(T, Long)], PriorityQueue[T]] {
  val outlet = Outlet[(T, Long)]("PriorityQueueWithRefresh.out")
  val shape: SourceShape[(T, Long)] = SourceShape(outlet)

  def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, PriorityQueue[T]) = {
    object Logic extends GraphStageLogic(shape) with PriorityQueue[T] with OutHandler {
      setHandler(outlet, this)

      val callback = getAsyncCallback[T] { t =>
        processOffer(t)
      }
      val refreshCallback = getAsyncCallback[T => Boolean] { p =>
        queue.mapInPlace {
          case (t, _) if p(t) =>
            println(s"Reordering $t to front")
            t -> System.nanoTime()
          case x => x
        }
      }

      val queue: mutable.PriorityQueue[(T, Long)] = mutable.PriorityQueue.empty[(T, Long)](Ordering.by[(T, Long), Long](_._2))

      def processOffer(t: T): Unit =
        if (isAvailable(outlet)) push(outlet, t -> System.nanoTime())
        else queue.enqueue(t -> System.nanoTime())

      def onPull(): Unit =
        if (queue.nonEmpty)
          push(outlet, queue.dequeue())

      def offer(t: T): Unit = callback.invoke(t)
      def refresh(p: T => Boolean): Unit = refreshCallback.invoke(p)
    }

    (Logic, Logic)
  }
}

trait PriorityQueue[T] {
  def offer(t: T): Unit
  def refresh(t: T => Boolean): Unit
}
object PriorityQueue {
  def queue[T](): Source[(T, Long), PriorityQueue[T]] =
    Source.fromGraph(new PriorityQueueWithRefreshStage[T])
}