package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ ContentTypes, HttpEntity, HttpHeader, HttpMethods, HttpRequest, HttpResponse, RequestEntity, StatusCodes, headers }
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import org.apache.pekko.http.scaladsl.marshalling.Marshal
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
import org.apache.pekko.stream.scaladsl.{ FileIO, Sink, Source }
import org.apache.pekko.util.ByteString

import scala.concurrent.{ ExecutionContext, Future }
import java.io.{ BufferedOutputStream, File, FileOutputStream }
import java.util.concurrent.atomic.AtomicLong

object VesuviusWorkerMain extends App {
  Console.println(s"Booting up Vesuvius worker version ${BuildInfo.version} built at ${BuildInfo.builtAtString}")

  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  val config = WorkerConfig.fromConfig(system.settings.config)
  def contextFor(item: WorkItem): WorkContext = WorkContext(system, config, log(_, item.id))

  def runWorkItem(item: WorkItem): Future[(File, WorkItemResult)] = {
    implicit val ctx = contextFor(item)
    item.input match {
      case i: InferenceWorkItemInput       => Tasks.infer(item, i)
      case p @ PPMFingerprintWorkItemInput => Tasks.ppmFingerprint(item)
      case d: DownsamplePPMWorkItemInput   => Tasks.downsamplePpm(item, d)
    }
  }

  val auth = headers.Authorization(headers.BasicHttpCredentials(config.dataServerUsername, config.dataServerPassword))

  val nextWorkEndpoint = s"${config.workEndpoint}/next?workerId=${config.workerId}&workTypes=${config.supportedWorkTypes.mkString(",")}"
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
          .map { res =>
            // cleanup workdir
            val workDir = new File(config.dataDir, workItem.id)
            import sys.process._
            if (workDir.exists)
              s"rm -r ${workDir.getAbsolutePath}".!!
            res
          }
      }

  def worker: Future[Any] =
    runOne()
      .recover {
        case ex =>
          // we currently get 302s from the server when it has no work, we handle that silently
          if (!ex.getMessage.contains("Unexpected status code 302 Found")) {
            println(s"Error: $ex")
            ex.printStackTrace()
            println("Backing off for a while")
          } else
            Console.println("No work, sleeping for 10s")
          Thread.sleep(10000)
          ()
      }
      .flatMap(_ => worker)

  worker.onComplete { res =>
    println(s"Worker stopped: $res")
  }
}

trait WorkContext {
  implicit def system: ActorSystem
  implicit def ec: ExecutionContext = system.dispatcher
  def println(msg: String): Unit
  def config: WorkerConfig

  lazy val DataServerAuthorizationHeader: HttpHeader =
    headers.Authorization(headers.BasicHttpCredentials(config.dataServerUsername, config.dataServerPassword))
}
object WorkContext {
  def apply(_system: ActorSystem, _config: WorkerConfig, _println: String => Unit): WorkContext = new WorkContext {
    override implicit def system: ActorSystem = _system
    override def println(msg: String): Unit = _println(msg)
    override def config: WorkerConfig = _config
  }
}

object Tasks {
  def infer(item: WorkItem, input: InferenceWorkItemInput)(implicit ctx: WorkContext): Future[(File, WorkItemResult)] = {
    import ctx._

    Future {
      def downloadLayers(segment: SegmentReference, from: Int, num: Int, to: File, reverse: Boolean): Future[File] =
        Source(from until from + num)
          .mapAsync(config.concurrentDownloads) { layer =>
            val targetId = if (reverse) from + num - 1 - (layer - from) else layer
            val targetFile = new File(to, f"layers/$targetId%02d.tif")
            download(
              segment.layerUrl(layer),
              targetFile,
              Some(DataServerAuthorizationHeader)
            )
          }
          .runWith(Sink.ignore)
          .map(_ => to)

      val workDir = new File(config.dataDir, item.id)
      workDir.mkdirs()
      val segmentBaseDir = config.dataDir
      val segmentDir = new File(segmentBaseDir, item.segment.segmentId)
      segmentDir.mkdirs()

      val (runInference: (() => Future[(File, WorkItemResult)]), numLayers: Int, modelDownload: String, modelTarget: File) =
        input.model match {
          case "youssef-test" =>
            val inferenceScriptDir = new File(config.inferenceScriptDir, "first-letters")
            if (inferenceScriptDir.exists()) {
              println("Fetching latest version of model")
              import sys.process._
              s"""git -C $inferenceScriptDir fetch""".!(ProcessLogger(println))
              s"""git -C $inferenceScriptDir checkout origin/worker""".!(ProcessLogger(println))
            }

            val inferenceScript = new File(inferenceScriptDir, "inference.py")
            val model = new File(config.inferenceScriptDir, "model.ckpt") // model checkpoint itself is one level up
            //require(model.exists, s"model checkpoint does not exist at ${model.getAbsolutePath}")

            def runInference(): Future[(File, WorkItemResult)] = Future {
              import sys.process._
              require(model.exists, s"model checkpoint does not exist at ${model.getAbsolutePath}, ls: ${s"ls -lash ${model.getAbsolutePath}".!!}, ls dir: ${s"ls -lash ${config.inferenceScriptDir.getAbsolutePath}".!!}")
              val cmdLine = s"python3 ${inferenceScript.getAbsolutePath} --model_path ${model.getAbsolutePath} --out_path ${workDir.getAbsolutePath} --segment_path ${segmentBaseDir.getAbsolutePath} --segment_id ${item.segment.segmentId} --stride ${input.stride} --start_idx ${input.startLayer} --workers 6"
              println(s"Running [$cmdLine]")

              val res = cmdLine.!!(ProcessLogger(println))
              val outputFile = new File(workDir, s"${item.segment.segmentId}_${input.stride}_${input.startLayer}.png")
              require(outputFile.exists, s"Output file $outputFile does not exist")
              println(s"Output file $outputFile exists")
              s"rm -r ${segmentDir.getAbsolutePath}".!!
              (outputFile, WorkCompleted(item, res))
            }

            (() => runInference(), 30, "https://f004.backblazeb2.com/file/bulk-data-jr/model.ckpt", model)

          case x if x `startsWith` "grand-prize" =>
            val inferenceScriptDir = new File(config.inferenceScriptDir, "grand-prize")
            if (inferenceScriptDir.exists()) {
              println("Fetching latest version of model")
              import sys.process._
              s"""git -C $inferenceScriptDir fetch""".!(ProcessLogger(println))
              s"""git -C $inferenceScriptDir checkout origin/worker""".!(ProcessLogger(println))
            }

            // python3 inference_timesformer.py --segment_id 20230827161847 --segment_path $(pwd)/train_scrolls --model_path timesformer_wild15_20230702185753_0_fr_i3depoch=12.ckpt --stride 256 --workers=10

            val inferenceScript = new File(inferenceScriptDir, "inference_timesformer.py")
            val model = new File(config.inferenceScriptDir, s"${input.model}.ckpt") // model checkpoint itself is one level up

            def runInference(): Future[(File, WorkItemResult)] = Future {
              import sys.process._
              require(model.exists, s"model checkpoint does not exist at ${model.getAbsolutePath}, ls: ${s"ls -lash ${model.getAbsolutePath}".!!}, ls dir: ${s"ls -lash ${config.inferenceScriptDir.getAbsolutePath}".!!}")
              val cmdLine = s"python3 ${inferenceScript.getAbsolutePath} --model_path ${model.getAbsolutePath} --out_path ${workDir.getAbsolutePath} --segment_path ${segmentBaseDir.getAbsolutePath} --segment_id ${item.segment.segmentId} --stride ${input.stride} --start_idx ${input.startLayer} --workers 6"
              println(s"Running [$cmdLine]")

              val res = cmdLine.!!(ProcessLogger(println))
              val outputFile = new File(workDir, s"${item.segment.segmentId}_${input.stride}_${input.startLayer}.png")
              require(outputFile.exists, s"Output file $outputFile does not exist")
              println(s"Output file $outputFile exists")
              s"rm -r ${segmentDir.getAbsolutePath}".!!
              (outputFile, WorkCompleted(item, res))
            }

            val modelFileUrl = input.model match {
              case "grand-prize"           => "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_wild15_20230702185753_0_fr_i3depoch=12.ckpt"
              case "grand-prize-finetune0" => "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_4972_epoch_3_by_valid_loss_0.704.ckpt"
              case "grand-prize-finetune1" => "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_9800_by_train_loss_0.575.ckpt"
              case "grand-prize-finetune2" => "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_8701_epoch_6_by_valid_loss_0.702.ckpt"
              case "grand-prize-finetune3" => "https://f004.backblazeb2.com/file/bulk-data-jr/timesformer_valid_Frag5-right_step_14900_by_train_loss_0.556.ckpt"
            }

            (() => runInference(), 26, modelFileUrl, model)

          case x =>
            throw new IllegalArgumentException(s"Unsupported model $x")
        }

      println(s"Working on $item")
      for {
        model <- download(modelDownload, modelTarget, None)
        maskFileName = s"${item.segment.segmentId}_mask.png"
        mask <- download(f"${item.segment.baseUrl}$maskFileName", new File(segmentDir, maskFileName), Some(DataServerAuthorizationHeader))
        res <- downloadLayers(item.segment, input.startLayer, numLayers, segmentDir, input.reverseLayers)
        inference <- runInference()
      } yield inference
    }.flatten
  }

  def ppmFingerprint(item: WorkItem)(implicit ctx: WorkContext): Future[(File, WorkItemResult)] = {
    import ctx._
    val workDir = new File(config.dataDir, item.id)
    val segmentDir = new File(workDir, item.segment.segmentId)
    segmentDir.mkdirs()

    val resultTarget = new File(segmentDir, s"${item.segment.segmentId}.fingerprint.json")
    val target = new File(segmentDir, s"${item.segment.segmentId}.ppm")
    val srcUrl = f"${item.segment.baseUrl}${item.segment.segmentId}.ppm"
    download(srcUrl, target, Some(DataServerAuthorizationHeader))
      .map { res =>
        val fingerprint = PPMFingerprinter.fingerprint(item.segment, res)
        val json = write(fingerprint, resultTarget)
        (json, WorkCompleted(item, "Fingerprinting complete"))
      }
  }

  def downsamplePpm(item: WorkItem, input: DownsamplePPMWorkItemInput)(implicit ctx: WorkContext): Future[(File, WorkItemResult)] = {
    import ctx._
    val workDir = new File(config.dataDir, item.id)
    val segmentDir = new File(workDir, item.segment.segmentId)
    segmentDir.mkdirs()

    val resultTarget = new File(segmentDir, s"${item.segment.segmentId}.ppm.bin")
    val target = new File(segmentDir, s"${item.segment.segmentId}.ppm")
    val srcUrl = f"${item.segment.baseUrl}${item.segment.segmentId}.ppm"
    download(srcUrl, target, Some(DataServerAuthorizationHeader))
      .map { ppmFile =>
        implicit val ppm = PPMReader(ppmFile)
        val dtpeSize = input.positionType match {
          case "u16" => 2
        }

        val size = dtpeSize * 3 * (ppm.width >> input.downsamplingBits) * (ppm.height >> input.downsamplingBits)
        println(s"Downsampling ${ppm.width}x${ppm.height} to ${ppm.width >> input.downsamplingBits}x${ppm.height >> input.downsamplingBits} (${size}B)")

        val fos = new BufferedOutputStream(new FileOutputStream(resultTarget))
        def u16le(v: Int): Unit = {
          fos.write(v & 0xff)
          fos.write((v >> 8) & 0xff)
        }
        for {
          v <- 0 until (ppm.height >> input.downsamplingBits)
          u <- 0 until (ppm.width >> input.downsamplingBits)
        } {
          val uv = UV(u << input.downsamplingBits, v << input.downsamplingBits)
          u16le(uv.x)
          u16le(uv.y)
          u16le(uv.z)
        }
        fos.close()
        require(resultTarget.length() == size, s"Result file has wrong size: ${resultTarget.length()} != $size")

        (resultTarget, WorkCompleted(item, "Fingerprinting complete"))
      }
  }

  import spray.json._
  def write[T: JsonFormat](value: T, target: File): File = {
    val fos = new FileOutputStream(target)
    fos.write(value.toJson.compactPrint.getBytes("utf8"))
    fos.close()
    target
  }

  val totalDownloaded = new AtomicLong()
  val lastDownloadReport = new AtomicLong(System.currentTimeMillis())
  val lastReportDownloaded = new AtomicLong()
  val reportDownloadMillis = 10000
  def countBytes(bytes: ByteString)(implicit ctx: WorkContext): ByteString = {
    import ctx.println
    val total = totalDownloaded.addAndGet(bytes.size)
    val last = lastDownloadReport.get()
    val now = System.currentTimeMillis()
    if (last + reportDownloadMillis < now) {
      if (lastDownloadReport.compareAndSet(last, now)) { // we won the race
        val old = lastReportDownloaded.getAndSet(total)
        val downloaded = total - old
        val lastedMillis = now - last
        println(f"Global download counter ${downloaded.toDouble / 1024d / 1024d}%6.3fMB. Thpt: ${downloaded / 1024d / 1024d * 1000d / lastedMillis}%5.2fMB/s")
      }
    }
    bytes
  }
  def download(url: String, to: File, authorizationHeader: Option[HttpHeader])(implicit ctx: WorkContext): Future[File] = if (to.exists()) {
    import ctx._
    println(s"Skipping download of $url to $to, already exists")
    Future.successful(to)
  } else {
    import ctx._
    println(s"Downloading $url to ${to.getAbsolutePath}")
    to.getParentFile.mkdirs()
    val tmpFile = new File(to.getParentFile, s".tmp-${to.getName}")
    Http().singleRequest(HttpRequest(HttpMethods.GET, uri = url, headers = authorizationHeader.toSeq))
      .flatMap { res =>
        val start = System.nanoTime()
        if (res.status == StatusCodes.OK)
          res.entity.dataBytes
            .map(countBytes)
            .runWith(FileIO.toPath(tmpFile.toPath)).map(_ => start)
        else {
          val text = Unmarshal(res).to[String]
          text.map { t =>
            throw new RuntimeException(s"Got status ${res.status} for $url: '$t'")
          }
        }
      }
      .map { start =>
        val end = System.nanoTime()
        val lastedMillis = (end - start) / 1000000
        println(f"Download of $url complete. Size: ${tmpFile.length().toDouble / 1024d / 1024d}%6.3fMB Took ${lastedMillis / 1000}s. Thpt: ${tmpFile.length().toDouble / 1024d / 1024d * 1000d / lastedMillis}%5.2fMB/s")
        tmpFile.renameTo(to);
        to
      }
  }
}