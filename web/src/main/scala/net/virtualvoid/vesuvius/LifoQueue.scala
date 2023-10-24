package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.{ Attributes, Outlet, SourceShape }
import org.apache.pekko.stream.scaladsl.{ Sink, Source, SourceQueue }
import org.apache.pekko.stream.stage.{ GraphStageLogic, GraphStageWithMaterializedValue, OutHandler }

import scala.collection.mutable
import scala.concurrent.{ Future, Promise }
import scala.util.{ Success, Try }

class LifoQueueStage[T] extends GraphStageWithMaterializedValue[SourceShape[T], LifoQueue[T]] {
  val outlet = Outlet[T]("LifoQueue.out")
  val shape: SourceShape[T] = SourceShape(outlet)

  def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, LifoQueue[T]) = {
    object Logic extends GraphStageLogic(shape) with LifoQueue[T] with OutHandler {
      setHandler(outlet, this)

      val callback = getAsyncCallback[T] { t =>
        processOffer(t)
      }

      val queue: mutable.Stack[T] = mutable.Stack.empty

      def processOffer(t: T): Unit =
        if (isAvailable(outlet)) push(outlet, t)
        else queue.push(t)

      def onPull(): Unit =
        if (queue.nonEmpty) push(outlet, queue.pop())

      def offer(t: T): Unit = callback.invoke(t)
    }

    (Logic, Logic)
  }
}

trait LifoQueue[T] {
  def offer(t: T): Unit
}

object LifoQueue {
  def queue[T](): Source[T, LifoQueue[T]] =
    Source.fromGraph(new LifoQueueStage[T])

  def semaphore[T, U](parallelism: Int)(f: T => Future[U])(implicit system: ActorSystem): T => Future[U] = {
    import system.dispatcher
    val q =
      queue[(T, Promise[U])]()
        .mapAsync[(Try[U], Promise[U])](parallelism) {
          case (t, p: Promise[U]) =>
            f(t).transform(res => Success((res, p)))
        }
        .to(Sink.foreach {
          case (f, p) =>
            p.complete(f)
        })
        .run()

    t => {
      val p = Promise[U]()
      q.offer((t, p))
      p.future
    }
  }
}
