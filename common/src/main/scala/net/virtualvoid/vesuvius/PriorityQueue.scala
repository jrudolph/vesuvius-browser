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
          case (k, v, oldTimestamp, id) =>
            map.put(k, (k, v, timestamp, id))
            queue.remove((oldTimestamp, id))
            queue.put((timestamp, id), (k, v))
        }
      }

      var id = 0
      val map: mutable.HashMap[K, (K, V, Long, Long)] = mutable.HashMap.empty
      val queue: mutable.TreeMap[(Long, Long), (K, V)] = mutable.TreeMap.empty[(Long, Long), (K, V)](Ordering.by(x => (-x._1, x._2)))
      def dequeue(): (K, V, Long) = {
        val ((timestamp, id), (k, v)) = queue.head
        queue.remove((timestamp, id))
        map.remove(k)
        (k, v, timestamp)
      }

      def processOffer(kv: (K, V)): Unit =
        if (isAvailable(outlet)) push(outlet, (kv._1, kv._2, System.nanoTime()))
        else {
          val timestamp = System.nanoTime()
          id += 1
          map.put(kv._1, (kv._1, kv._2, timestamp, id))
          queue.put((timestamp, id), kv)
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