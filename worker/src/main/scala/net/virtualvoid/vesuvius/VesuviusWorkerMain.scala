package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse, RequestEntity, StatusCodes }
import org.apache.pekko.http.scaladsl.model.ws.{ Message, TextMessage, WebSocketRequest }
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import org.apache.pekko.http.scaladsl.marshalling.Marshal
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
import org.apache.pekko.stream.scaladsl.{ FileIO, Flow }

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration.*
import spray.json.*

import java.io.File

object VesuviusWorkerMain extends App {
  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  val config = WorkerConfig.fromConfig(system.settings.config)

  def runWorkItem(item: WorkItem): Future[(File, WorkItemResult)] =
    item match {
      case i: InferenceWorkItem => Tasks.infer(config, i)
    }

  /*def worker: Flow[WorkItem, WorkItemResult, Any] =
    Flow[WorkItem].mapAsyncUnordered(1)(runWorkItem)

  def wsFlow: Flow[Message, Message, Any] =
    Flow[Message]
      .collect { case msg: TextMessage => msg }
      .mapAsync(1)(_.toStrict(10.seconds))
      .map(_.text.parseJson.convertTo[WorkItem])
      .via(worker)
      .map(x => TextMessage(x.toJson.compactPrint))

  val workItemEndpoint = "http://localhost:8089/tasks"

  Http()
    .singleWebSocketRequest(
      WebSocketRequest(uri = workItemEndpoint),
      clientFlow = wsFlow
    )*/

  val nextWorkEndpoint = s"${config.workEndpoint}/next?workerId=${config.workerId}"
  def resultEndpoint(workItemId: Int) = s"${config.workEndpoint}/result?workerId=${config.workerId}&workId=$workItemId"
  val completeEndpoint = s"${config.workEndpoint}/complete?workerId=${config.workerId}"

  def post(endpoint: String, data: RequestEntity = HttpEntity.Empty): Future[HttpResponse] =
    Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = endpoint, entity = data))
      .flatMap { res =>
        if (res.status == StatusCodes.OK)
          Future.successful(res)
        else
          Future.failed(new RuntimeException(s"Unexpected status code ${res.status} for endpoint $endpoint"))
      }

  def runOne(): Future[Any] =
    post(nextWorkEndpoint)
      .flatMap(Unmarshal(_).to[WorkItem])
      .flatMap(runWorkItem)
      .flatMap {
        case (data: File, result: WorkItemResult) =>
          if (data.exists)
            post(resultEndpoint(result.workItem.id), HttpEntity(ContentTypes.`application/octet-stream`, FileIO.fromPath(data.toPath)))
              .map(_ => result)
          else Future.successful(result)
      }
      .flatMap(Marshal(_).to[RequestEntity])
      .flatMap(post(completeEndpoint, _))

  def worker: Future[Any] =
    runOne()
      .recover {
        case ex =>
          println(s"Error: $ex")
          ex.printStackTrace()
          println("Backing off for a while")
          Thread.sleep(10000)
          ()
      }
      .flatMap(_ => worker)

  worker.onComplete { res =>
    println(s"Worker stopped: $res")
  }
}

object Tasks {
  def infer(config: WorkerConfig, item: InferenceWorkItem)(implicit ec: ExecutionContext): Future[(File, WorkItemResult)] = Future {
    println(s"Working on $item")
    Thread.sleep(5000)
    val f = new File("../out.png")
    require(f.exists)
    f -> WorkCompleted(item, "Everything is fine!")
  }
}