package net.virtualvoid.vesuvius

import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.stream.stage.{ GraphStageLogic, GraphStageWithMaterializedValue, OutHandler }
import org.apache.pekko.stream.{ Attributes, Outlet, SourceShape }

import scala.collection.mutable

class PriorityQueueWithRefreshStage[K, V] extends GraphStageWithMaterializedValue[SourceShape[(K, V, Long)], PriorityQueue[K, V]] {
  val outlet = Outlet[(K, V, Long)]("PriorityQueueWithRefresh.out")
  val shape: SourceShape[(K, V, Long)] = SourceShape(outlet)

  def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, PriorityQueue[K, V]) = {
    object Logic extends GraphStageLogic(shape) with PriorityQueue[K, V] with OutHandler {
      setHandler(outlet, this)

      val callback = getAsyncCallback[(K, V)](processOffer)

      val refreshCallback = getAsyncCallback[K] { key =>
        val timestamp = System.nanoTime()
        map.get(key).foreach {
          case (k, v, oldTimestamp) =>
            map.put(k, (k, v, timestamp))
            queue.remove(oldTimestamp)
            queue.put(timestamp, (k, v))
        }
      }

      val map: mutable.HashMap[K, (K, V, Long)] = mutable.HashMap.empty
      val queue: mutable.TreeMap[Long, (K, V)] = mutable.TreeMap.empty[Long, (K, V)](Ordering.by(-_))
      def dequeue(): (K, V, Long) = {
        val (timestamp, (k, v)) = queue.head
        queue.remove(timestamp)
        map.remove(k)
        (k, v, timestamp)
      }

      def processOffer(kv: (K, V)): Unit =
        if (isAvailable(outlet)) push(outlet, (kv._1, kv._2, System.nanoTime()))
        else {
          val timestamp = System.nanoTime()
          map.put(kv._1, (kv._1, kv._2, timestamp))
          queue.put(timestamp, kv)
        }

      def onPull(): Unit =
        if (queue.nonEmpty) {
          push(outlet, dequeue())
          if (queue.size % 10 == 0) println(s"Queue size: ${queue.size}")
        }

      def offer(key: K, value: V): Unit = callback.invoke((key, value))
      def refresh(key: K): Unit = refreshCallback.invoke(key)
    }

    (Logic, Logic)
  }
}

trait PriorityQueue[K, V] {
  def offer(key: K, value: V): Unit
  def refresh(key: K): Unit
}
object PriorityQueue {
  def queue[K, V](): Source[(K, V, Long), PriorityQueue[K, V]] =
    Source.fromGraph(new PriorityQueueWithRefreshStage[K, V])
}