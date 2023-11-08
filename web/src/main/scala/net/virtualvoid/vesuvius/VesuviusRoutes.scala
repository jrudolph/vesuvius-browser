package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage}
import org.apache.pekko.http.scaladsl.model.{HttpMethods, HttpRequest, StatusCodes, Uri, headers}
import org.apache.pekko.http.scaladsl.server.{Directive1, Directives, Route}
import org.apache.pekko.stream.scaladsl.{FileIO, Flow, Sink, Source}
import play.twirl.api.Html
import spray.json.*

import java.io.File
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Success

case class DZISize(Width: Int, Height: Int)
case class DZIImage(
    xmlns:    String, // constant: "http://schemas.microsoft.com/deepzoom/2008"
    Format:   String,
    Overlap:  Int,
    TileSize: Int,
    Size:     DZISize)

object DZIImage {
  import DefaultJsonProtocol._

  implicit val sizeFormat: RootJsonFormat[DZISize] = jsonFormat2(DZISize.apply _)
  implicit val imageFormat: RootJsonFormat[DZIImage] = jsonFormat5(new DZIImage(_, _, _, _, _))

  def apply(format: String, overlap: Int, tileSize: Int, width: Int, height: Int): DZIImage =
    DZIImage("http://schemas.microsoft.com/deepzoom/2008", format, overlap, tileSize, DZISize(width, height))
}

class VesuviusRoutes(config: AppConfig)(implicit system: ActorSystem) extends Directives with TwirlSupport with SprayJsonSupport {

  import system.dispatcher
  import config.dataDir

  val cpuBound = system.dispatchers.lookup("cpu-bound-dispatcher")

  val userManagement = UserManagement(config)

  lazy val main =
    encodeResponse { mainRoute }

  val NameR = """(\d+)_(\d+).jpg""".r

  lazy val scrollSegments: Future[Seq[ImageInfo]] =
    for {
      id1 <- segmentIds(FullScrollsBase, 1)
      id2 <- segmentIds(FullScrollsBase, 2)
      id3 <- segmentIds(PHercBase, 332)
      infos <- Future.traverse(id1 ++ id2 ++ id3)(imageInfo)
    } yield infos.flatten.sortBy(i => (i.scroll, i.segmentId))

  lazy val scrollSegmentsMap: Future[Map[(Int, String), SegmentReference]] =
    scrollSegments.map(_.map(i => (i.scroll, i.segmentId) -> i.ref).toMap)

  def resolveRef(scroll: Int, segmentId: String): Directive1[SegmentReference] =
    scrollSegmentsMap.map(_.get((scroll, segmentId))).await.orReject

  lazy val mainRoute =
    concat(
      pathSingleSlash {
        get {
          scrollSegments.await { infos =>
            page(html.home(infos))
          }
        }
      },
      path("login") {
        concat(
          post {
            formField("user", "password") { (user, password) =>
              userManagement.login(user, password)
            }
          },
          get {
            page(html.login())
          }
        )
      },
      path("logout") {
        userManagement.logout
      },
      path("license") {
        page(html.license())
      },
      pathPrefix("scroll" / IntNumber / "segment" / Segment) { (scroll, segmentId) =>
        resolveRef(scroll, segmentId) { segment =>

          imageInfo(segment).await.orReject { (info: ImageInfo) =>
            concat(
              pathSingleSlash {
                page(html.segment(info))
              },
              path("mask") {
                resizedMask(segment).deliver
              },
              pathPrefix("inferred" / Segment) { model =>
                val input = model match {
                  // FIXME: use constants instead
                  case "youssef-test" if segment.scroll == 332          => requestedWorkInputs(2)._1.asInstanceOf[InferenceWorkItemInput]
                  case "youssef-test-reversed" if segment.scroll == 332 => requestedWorkInputs(3)._1.asInstanceOf[InferenceWorkItemInput]
                  case "youssef-test"                                   => requestedWorkInputs(0)._1.asInstanceOf[InferenceWorkItemInput]
                  case "youssef-test-reversed"                          => requestedWorkInputs(1)._1.asInstanceOf[InferenceWorkItemInput]
                }
                concat(
                  pathEnd {
                    concat(
                      resizedInferred(segment, input).await.orReject.deliver,
                      complete(ImageTools.EmptyImageResponse)
                    )
                  },
                  path("full") {
                    parameter("show".?) { show =>
                      val inner = getFromFile(targetFileForInput(segment, input))
                      if (show.isDefined) inner
                      else
                        respondWithHeader(headers.`Content-Disposition`(headers.ContentDispositionTypes.attachment, Map("filename" -> s"${segment.segmentId}_inference_${model}.png"))) {
                          inner
                        }
                    }
                  }
                )
              },
              path("ppm") {
                println(s"at ppm for $segment")
                val ppmFile = targetFileForInput(segment, DownSampleU16_2Input)
                if (ppmFile.exists()) {
                  val reader = SmallPPMReader(ppmFile, DownSampleU16_2Input, info.width, info.height)
                  val Pattern = """(\d+),(\d+)""".r
                  println(s"PPM reader: $info")
                  handleWebSocketMessages(
                    Flow[Message]
                      .mapAsync(1) {
                        case t: TextMessage =>
                          t.toStrict(2.second).map(_.text)
                      }
                      .collect {
                        case Pattern(uStr, vStr) =>
                          val u = uStr.toInt
                          val v = vStr.toInt
                          val (x, y, z) = reader.xyz(u, v)
                          val res = s"$u,$v,$x,$y,$z"
                          //println(res)
                          TextMessage(res)
                      }
                  )
                } else reject
              },
              pathPrefix(IntNumber) { z =>
                concat(
                  path("dzi") {
                    val dziImage = DZIImage("jpg", 0, 512, info.width, info.height) // FIXME: imagesize
                    complete(JsObject("Image" -> dziImage.toJson))
                  },
                  path("dzi_files" / IntNumber / Segment) { (layer, name) =>
                    val NameR(xStr, yStr) = name
                    val x = xStr.toInt
                    val y = yStr.toInt

                    resizer(info, layer, x, y, z).deliver
                  }
                )
              }
            )
          }
        }
      },
      pathPrefix("work") {
        concat(
          post {
            concat(
              path("next") {
                parameter("workerId", "workTypes") { (workerId, workType) =>
                  val allowed = workType.split(',').toSet
                  workItemManager.await { man =>
                    val item = man.assignNext(workerId, allowed)
                    println(s"Assigned $item to $workerId")
                    provide(item).orReject(complete(_))
                  }
                }
              },
              path("log") {
                parameter("workerId", "workId".?) { (workerId, workItemId) =>
                  entity(as[String]) { log =>
                    println(s"[$workerId${workItemId.map("/" + _).getOrElse("")}]: $log")
                    complete("OK")
                  }
                }
              },
              path("result") {
                parameter("workerId", "workId".as[String]) { (workerId, workItemId) =>
                  workItemManager.await { man =>
                    provide(man.findItem(workItemId)).orReject {
                      case item @ WorkItem(_, _, _, input) =>
                        println(s"Got result data request for $workItemId from $workerId, item: $item")

                        val file = targetFileForInput(item.segment, input)
                        file.getParentFile.mkdirs()
                        val tmpFile = File.createTempFile(".tmp.result", ".tmp", file.getParentFile)

                        extractRequestEntity { entity =>
                          val result =
                            entity.dataBytes
                              .runWith(FileIO.toPath(tmpFile.toPath))
                              .map { _ => tmpFile.renameTo(file); "OK" }

                          complete(result)
                        }
                    }
                  }
                }
              },
              path("complete") {
                parameter("workerId") { workerId =>
                  entity(as[WorkItemResult]) { result =>
                    println(s"Got result: $result from $workerId")
                    workItemManager.await { man =>
                      man.markDone(workerId, result.workItem)
                      complete("OK")
                    }
                  }
                }
              }
            )
          },
          path("app")(getFromResource("app.jar")),
          path("deps")(depsFile(getFromFile)),
          adminRoutes
        )
      },
      getFromResourceDirectory("web")
    )

  def adminRoutes =
    userManagement.ensureAdmin {
      get {
        pathEnd {
          workItemManager.await { manager =>
            page(html.work(manager.itemStates.toSeq))
          }
        }
      }
    }

  def page(html: Html): Route =
    userManagement.loggedIn { implicit user =>
      complete(html)
    }

  val InfoCache = LfuCache[SegmentReference, Option[ImageInfo]]

  def imageInfo(segment: SegmentReference): Future[Option[ImageInfo]] =
    InfoCache.getOrLoad(segment, _ => _imageInfo(segment))

  //areaFor
  def sizeOf(segment: SegmentReference): Future[(Int, Int)] =
    maskFor(segment)
      .map { f =>
        import sys.process._
        val cmd = s"vipsheader -a $f"
        val output = cmd.!!
        val kvs = output.split('\n').map(_.split(": ").map(_.trim)).map(a => a(0) -> a(1)).toMap
        val width = kvs("width").toInt
        val height = kvs("height").toInt
        (width, height)
      }(cpuBound)

  def _imageInfo(segment: SegmentReference): Future[Option[ImageInfo]] = {
    for {
      (width, height) <- sizeOf(segment)
      area <- areaFor(segment)
    } yield ImageInfo(segment, width, height, area)
  }.transform(x => Success(x.toOption))

  val SegmentLayerCache = LfuCache[(SegmentReference, Int), File]

  def segmentLayer(segment: SegmentReference, layer: Int): Future[File] =
    SegmentLayerCache.getOrLoad((segment, layer), _ => _segmentLayer(segment, layer))

  def _segmentLayer(segment: SegmentReference, layer: Int): Future[File] = {
    import segment._
    val targetFile = new File(dataDir, s"raw/scroll$scroll/$segmentId/layers/$layer.jp2")
    targetFile.getParentFile.mkdirs()

    if (targetFile.exists) Future.successful(targetFile)
    else if (layer == 2342) Future.successful(new File(dataDir, s"inferred/scroll$scroll/$segmentId/inference_youssef-test_15_32.png"))
    else if (layer == 2343) Future.successful(new File(dataDir, s"inferred/scroll$scroll/$segmentId/inference_youssef-test_15_32_reverse.png"))
    else if (layer == 2350) Future.successful(new File(dataDir, s"inferred/scroll$scroll/$segmentId/inference_youssef-test_63_32.png"))
    else if (layer == 2351) Future.successful(new File(dataDir, s"inferred/scroll$scroll/$segmentId/inference_youssef-test_63_32_reverse.png"))
    else {
      val webpVersion = new File(dataDir, s"raw/scroll$scroll/$segmentId/layers/$layer.webp")

      val tmpFile: Future[File] =
        if (webpVersion.exists())
          Future.successful(webpVersion)
        else {
          val url = segment.layerUrl(layer)
          val tmpFile = File.createTempFile(".tmp.download", ".tif", targetFile.getParentFile)
          download(url, tmpFile)
        }

      tmpFile
        .map { tmpFile =>
          val tmpFile2 = File.createTempFile(".tmp.convert", ".jp2", targetFile.getParentFile)
          import sys.process._
          val cmd = s"""vips copy $tmpFile "$tmpFile2[lossless]""""
          println(s"Convert big image to jp2: $cmd")
          cmd.!!
          targetFile.getParentFile.mkdirs()
          require(tmpFile2.renameTo(targetFile))
          tmpFile.delete()
          targetFile
        }(cpuBound)
    }
  }

  def resizer(info: ImageInfo, layer: Int, tileX: Int, tileY: Int, z: Int): Future[File] =
    dzdir(info, z).map { dir =>
      new File(dir, s"$layer/${tileX}_$tileY.jpg")
    }

  val DZDirCache = LfuCache[(ImageInfo, Int), File]

  def dzdir(info: ImageInfo, z: Int): Future[File] =
    DZDirCache.getOrLoad((info, z), _ => extractDZ(info, z))

  def extractDZ(info: ImageInfo, z: Int): Future[File] = {
    val targetDir = new File(dataDir, s"tiles-dz/scroll${info.scroll}/${info.segmentId}/layers/${z}_files")
    if (targetDir.exists()) Future.successful(targetDir)
    else segmentLayer(info.ref, z).map { imageFile =>
      targetDir.getParentFile.mkdirs()
      import sys.process._
      val tmpFile = File.createTempFile(s".tmp.$z", "", targetDir.getParentFile)
      val cmd = s"""vips dzsave $imageFile $tmpFile --suffix .jpg --tile-size 512 --overlap 0"""
      println(s"Extracting DZ: $cmd")
      cmd.!!
      val tmpDir = new File(tmpFile.getParentFile, tmpFile.getName + "_files")
      require(tmpDir.renameTo(targetDir))
      targetDir
    }(cpuBound)
  }

  def resizedMask(segment: SegmentReference): Future[File] =
    imageInfo(segment).flatMap { info =>
      import segment._
      resizedX(maskFor(segment), new File(dataDir, s"raw/scroll$scroll/$segmentId/mask_small_autorotated.png"), height = 100, rotate90 = !info.get.isLandscape).map(_.get)
    }

  def resizedInferred(segment: SegmentReference, input: InferenceWorkItemInput): Future[Option[File]] =
    imageInfo(segment).flatMap { i =>
      val file = targetFileForInput(segment, input)
      resizedX(file, new File(file.getParentFile, file.getName.dropRight(4) + "_small_autorotated.png"), height = 100, rotate90 = !i.get.isLandscape)
    }

  def resizedX(orig: File, target: File, height: Int, rotate90: Boolean): Future[Option[File]] =
    resizedX(Future.successful(orig), target, height, rotate90)
  def resizedX(orig: Future[File], target: File, height: Int, rotate90: Boolean): Future[Option[File]] =
    orig.flatMap { f0 =>
      cached(target, negTtlSeconds = 10, isValid = f => f0.exists() && f0.lastModified() < f.lastModified()) { () =>
        val f = Option(f0).filter(_.exists).getOrElse(throw new RuntimeException(s"File $f0 does not exist"))
        import sys.process._
        val rotatedFile =
          if (rotate90) {
            val rotFile = File.createTempFile(".tmp.rotated", ".png", target.getParentFile)
            val cmd = s"""vips rot $f $rotFile d90"""
            cmd.!!
            rotFile.deleteOnExit()
            rotFile
          } else f

        val tmpFile = File.createTempFile(".tmp.resized", ".png", target.getParentFile)
        val cmd = s"""vips thumbnail $rotatedFile $tmpFile 10000 --height $height"""
        cmd.!!

        require(tmpFile.renameTo(target))
        Future.successful(target)
      }
    }.transform(x => Success(x.toOption))

  def maskFor(segment: SegmentReference): Future[File] = {
    import segment._
    cacheDownload(
      s"${segment.baseUrl}${segment.segmentId}_mask.png",
      new File(dataDir, s"raw/scroll$scroll/$segmentId/mask.png"))
  }

  def areaFor(segment: SegmentReference): Future[Option[Float]] = {
    import segment._
    cacheDownload(
      s"${segment.baseUrl}area_cm2.txt",
      new File(dataDir, s"raw/scroll$scroll/$segmentId/area_cm2.txt")
    )
      .map(f => scala.io.Source.fromFile(f).getLines().next().toFloat)
      .transform(x => Success(x.toOption))
  }

  def segmentIds(base: ScrollServerBase, scroll: Int): Future[Seq[SegmentReference]] =
    segmentIds(base.baseUrl(scroll), new File(config.dataDir, s"raw/scroll$scroll/${base}-path-listing.html"))
      .map(_.map(id => SegmentReference(scroll, id, base)))

  val LinkR = """.*href="(.*)/".*""".r
  def segmentIds(baseUrl: String, targetFile: File): Future[Seq[String]] =
    cacheDownload(baseUrl, targetFile, ttlSeconds = 3600).map { f =>
      scala.io.Source.fromFile(f).getLines().collect {
        case LinkR(segmentId) if !segmentId.startsWith("..") => segmentId
      }.toVector
    }

  val DefaultPositiveTtl = 3600 * 24 * 365
  val DefaultNegativeTtl = 7200

  def cacheDownload(url: String, to: File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl): Future[File] =
    cached(to, ttlSeconds, negTtlSeconds) { () => download(url, to) }

  def cached(to: File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl, isValid: File => Boolean = _ => true)(f: () => Future[File]): Future[File] = {
    to.getParentFile.mkdirs()
    val neg = new File(to.getParentFile, s".neg-${to.getName}")
    if (to.exists() && to.lastModified() + ttlSeconds * 1000 > System.currentTimeMillis() && isValid(to)) Future.successful(to)
    else if (neg.exists() && neg.lastModified() + negTtlSeconds * 1000 > System.currentTimeMillis() && isValid(neg)) Future.failed(new RuntimeException(s"Negatively cached"))
    else
      f().recoverWith {
        case t: Throwable =>
          neg.getParentFile.mkdirs()
          neg.delete()
          neg.createNewFile()
          Future.failed(t)
      }
  }

  val auth = headers.Authorization(headers.BasicHttpCredentials(config.dataServerUsername, config.dataServerPassword))

  def download(url: String, to: File): Future[File] = {
    println(s"Downloading $url")
    val tmpFile = new File(to.getParentFile, s".tmp-${to.getName}")
    Http().singleRequest(HttpRequest(HttpMethods.GET, Uri(url), headers = auth :: Nil))
      .flatMap { res =>
        require(res.status == StatusCodes.OK, s"Got status ${res.status} for $url")
        res.entity.dataBytes.runWith(FileIO.toPath(tmpFile.toPath))
      }
      .map { _ => tmpFile.renameTo(to); to }
  }

  def targetFileForInput(segment: SegmentReference, input: WorkItemInput): File = {
    import segment._
    input match {
      case InferenceWorkItemInput(model, startLayer, stride, reverseLayers) =>
        val reversed = if (reverseLayers) "_reverse" else ""
        new File(dataDir, s"inferred/scroll$scroll/$segmentId/inference_${model}_${startLayer}_${stride}$reversed.png")
      case PPMFingerprintWorkItemInput =>
        new File(dataDir, s"ppm/scroll${segment.scroll}/${segment.segmentId}/fingerprint.json")
      case DownsamplePPMWorkItemInput(tpe, downsamplingBits) =>
        new File(dataDir, s"ppm/scroll${segment.scroll}/${segment.segmentId}/uvmap-${tpe}-${downsamplingBits}.bin")
    }
  }

  val DownSampleU16_2Input = DownsamplePPMWorkItemInput("u16", 2)

  type Filter = SegmentReference => Boolean
  lazy val requestedWorkInputs: Seq[(WorkItemInput, Filter)] =
    Seq(
      InferenceWorkItemInput("youssef-test", 15, 32, false) -> (s => s.scroll == 1 || s.scroll == 2),
      InferenceWorkItemInput("youssef-test", 15, 32, true) -> (s => s.scroll == 1 || s.scroll == 2),
      InferenceWorkItemInput("youssef-test", 63, 32, false) -> (s => s.scroll == 332),
      InferenceWorkItemInput("youssef-test", 63, 32, true) -> (s => s.scroll == 332),
      PPMFingerprintWorkItemInput -> (_.scroll == 1),
      DownSampleU16_2Input -> (s => s.scroll == 1),
    )

  val runnerId = System.currentTimeMillis().toString
  lazy val workItems: Future[Seq[WorkItem]] =
    Source.futureSource(scrollSegments.map(x => Source(x.sortBy(_.segmentId).reverse)))
      .mapConcat { info =>
        requestedWorkInputs
          .filter(_._2(info.ref))
          .map(input => info.ref -> input._1)
      }
      .filter { case (ref, input) => !targetFileForInput(ref, input).exists() }
      .zipWithIndex
      .map { case ((ref, input), id) => WorkItem(s"$runnerId-$id", ref, input.`type`, input) }
      .runWith(Sink.seq)

  lazy val workItemManager: Future[WorkItemManager] = workItems.map(WorkItemManager(_))

  lazy val depsFile: Directive1[File] =
    provide(sys.props("java.class.path").split(":").find(_.contains("deps.jar")).map(new File(_))).orReject
}
