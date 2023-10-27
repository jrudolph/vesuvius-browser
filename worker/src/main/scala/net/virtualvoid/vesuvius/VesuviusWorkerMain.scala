package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse, RequestEntity, StatusCodes, headers }
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import org.apache.pekko.http.scaladsl.marshalling.Marshal
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
import org.apache.pekko.stream.scaladsl.{ FileIO, Sink, Source }

import scala.concurrent.Future
import java.io.File

object VesuviusWorkerMain extends App {
  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  val config = WorkerConfig.fromConfig(system.settings.config)

  def runWorkItem(item: WorkItem): Future[(File, WorkItemResult)] =
    item match {
      case i: InferenceWorkItem => Tasks.infer(config, i, log(_, item.id))
    }

  val auth = headers.Authorization(headers.BasicHttpCredentials(config.dataServerUsername, config.dataServerPassword))

  val nextWorkEndpoint = s"${config.workEndpoint}/next?workerId=${config.workerId}"
  def resultEndpoint(workItemId: String) = s"${config.workEndpoint}/result?workerId=${config.workerId}&workId=$workItemId"
  val completeEndpoint = s"${config.workEndpoint}/complete?workerId=${config.workerId}"
  def logEndpoint(workItemId: String) = s"${config.workEndpoint}/log?workerId=${config.workerId}&workId=$workItemId"

  def post(endpoint: String, data: RequestEntity = HttpEntity.Empty): Future[HttpResponse] =
    Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = endpoint, entity = data, headers = auth :: Nil))
      .flatMap { res =>
        if (res.status == StatusCodes.OK)
          Future.successful(res)
        else
          Future.failed(new RuntimeException(s"Unexpected status code ${res.status} for endpoint $endpoint"))
      }

  def log(msg: String, workId: String = ""): Future[HttpResponse] = {
    scala.Console.println(msg)
    post(logEndpoint(workId), HttpEntity(ContentTypes.`text/plain(UTF-8)`, msg))
  }

  def println(msg: String): Unit = log(msg)

  def runOne(): Future[Any] =
    post(nextWorkEndpoint)
      .flatMap(Unmarshal(_).to[WorkItem])
      .flatMap { workItem =>
        runWorkItem(workItem)
          .flatMap {
            case (data: File, result: WorkItemResult) =>
              if (data.exists)
                post(resultEndpoint(result.workItem.id), HttpEntity(ContentTypes.`application/octet-stream`, FileIO.fromPath(data.toPath)))
                  .map(_ => result)
              else Future.successful(result)
          }
          .flatMap(Marshal(_).to[RequestEntity])
          .flatMap(post(completeEndpoint, _))
          .recoverWith {
            case ex =>
              println(s"Error: $ex")
              ex.printStackTrace()
              Marshal(WorkFailed(workItem, ex.getMessage, "")).to[RequestEntity]
                .flatMap(post(completeEndpoint, _))
          }
      }

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
  def infer(config: WorkerConfig, item: InferenceWorkItem, println: String => Unit)(implicit system: ActorSystem): Future[(File, WorkItemResult)] = {
    import system.dispatcher

    Future {
      val auth = headers.Authorization(headers.BasicHttpCredentials(config.dataServerUsername, config.dataServerPassword))

      def downloadLayers(segment: SegmentReference, from: Int, num: Int, to: File): Future[File] =
        Source(from until from + num)
          .mapAsync(config.concurrentDownloads) { layer =>
            download(
              f"${segment.baseUrl}layers/$layer%02d.tif",
              new File(to, f"layers/$layer%02d.tif")
            )
          }
          .runWith(Sink.ignore)
          .map(_ => to)

      def download(url: String, to: File): Future[File] = {
        println(s"Downloading $url")
        to.getParentFile.mkdirs()
        val tmpFile = new File(to.getParentFile, s".tmp-${to.getName}")
        Http().singleRequest(HttpRequest(HttpMethods.GET, uri = url, headers = auth :: Nil))
          .flatMap { res =>
            val start = System.nanoTime()
            require(res.status == StatusCodes.OK, s"Got status ${res.status} for $url")
            res.entity.dataBytes
              .runWith(FileIO.toPath(tmpFile.toPath)).map(_ => start)
          }
          .map { start =>
            val end = System.nanoTime()
            val lastedMillis = (end - start) / 1000000
            println(f"Download of $url complete. Took ${lastedMillis / 1000}s. Thpt: ${tmpFile.length().toDouble / lastedMillis / 1000 / 1024 / 1024}%5.2fMB/s")
            tmpFile.renameTo(to);
            to
          }
      }

      val workDir = new File(config.dataDir, item.id.toString)
      val segmentDir = new File(workDir, item.segment.segmentId.toString)
      segmentDir.mkdirs()

      val inferenceScriptDir = config.inferenceScriptDir
      val inferenceScript = new File(inferenceScriptDir, "inference.py")
      val model = new File(inferenceScriptDir, "model.ckpt")
      require(model.exists, s"model checkpoint does not exist at ${model.getAbsolutePath}")
      def runInference(): Future[(File, WorkItemResult)] = Future {
        import sys.process._
        val cmdLine = s"python3 ${inferenceScript.getAbsolutePath} --model_path ${model.getAbsolutePath} --out_path ${workDir.getAbsolutePath} --segment_path ${workDir.getAbsolutePath} --segment_id ${item.segment.segmentId} --stride ${item.stride} --start_idx ${item.startLayer} --workers 6"
        println(s"Running [$cmdLine]")

        val res = cmdLine.!!(ProcessLogger(println))
        val outputFile = new File(workDir, s"${item.segment.segmentId}_${item.stride}_${item.startLayer}.png")
        require(outputFile.exists, s"Output file $outputFile does not exist")
        println(s"Output file $outputFile exists")
        s"rm -r ${segmentDir.getAbsolutePath}".!!
        (outputFile, WorkCompleted(item, res))
      }

      println(s"Working on $item")
      downloadLayers(item.segment, item.startLayer, 30, segmentDir)
        .flatMap { res =>
          val maskFileName = s"${item.segment.segmentId}_mask.png"
          download(f"${item.segment.baseUrl}$maskFileName", new File(segmentDir, maskFileName))
        }
        .flatMap { res =>
          println("Download complete, starting inference")
          runInference()
        }
    }.flatten
  }

}