package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.marshalling.{ Marshaller, ToResponseMarshallable }
import org.apache.pekko.http.scaladsl.model.ws.{ Message, TextMessage }
import org.apache.pekko.http.scaladsl.model.{ StatusCodes, headers }
import org.apache.pekko.http.scaladsl.server.{ Directive1, Directives, Route }
import org.apache.pekko.stream.scaladsl.{ FileIO, Flow, Sink, Source }
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
  val downloadUtils = new DownloadUtils(config)
  import downloadUtils._

  case class LayerDefinition(
      layerId:   Int,
      extension: String,
      layerBase: SegmentReference => Future[File]
  )
  /*
  else if (layer == 2342) Future.successful(new File(dataDir, s"inferred/scroll$scrollId/$segmentId/inference_youssef-test_15_32.png"))
  else if (layer == 2343) Future.successful(new File(dataDir, s"inferred/scroll$scrollId/$segmentId/inference_youssef-test_15_32_reverse.png"))
  else if (layer == 2350) Future.successful(new File(dataDir, s"inferred/scroll$scrollId/$segmentId/inference_youssef-test_63_32.png"))
  else if (layer == 2351) Future.successful(new File(dataDir, s"inferred/scroll$scrollId/$segmentId/inference_youssef-test_63_32_reverse.png"))
   */
  val GrandPrize17Layer = LayerDefinition(2344, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_grand-prize_17_32.png")))
  val GrandPrizeFinetune0_17Layer = LayerDefinition(2400, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_grand-prize-finetune0_17_32.png")))
  val GrandPrizeFinetune1_17Layer = LayerDefinition(2401, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_grand-prize-finetune1_17_32.png")))
  val GrandPrizeFinetune2_17Layer = LayerDefinition(2402, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_grand-prize-finetune2_17_32.png")))
  val GrandPrizeFinetune3_17Layer = LayerDefinition(2403, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_grand-prize-finetune3_17_32.png")))

  val Youssef15Layer = LayerDefinition(2342, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_youssef-test_15_32.png")))
  val Youssef15ReverseLayer = LayerDefinition(2343, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_youssef-test_15_32_reverse.png")))
  val Youssef63Layer = LayerDefinition(2350, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_youssef-test_63_32.png")))
  val Youssef63ReverseLayer = LayerDefinition(2351, "jpg", segment => Future.successful(new File(dataDir, s"inferred/scroll${segment.scrollId}/${segment.segmentId}/inference_youssef-test_63_32_reverse.png")))
  val InkLabelLayer = LayerDefinition(3000, "jpg", inklabelFor(_).map(_.get))
  val AlphaMaskLayer = LayerDefinition(9999, "png", alphaMaskFor)

  val layers =
    Seq(Youssef15Layer, Youssef15ReverseLayer, Youssef63Layer, Youssef63ReverseLayer, InkLabelLayer, AlphaMaskLayer, GrandPrize17Layer, GrandPrizeFinetune0_17Layer, GrandPrizeFinetune1_17Layer, GrandPrizeFinetune2_17Layer, GrandPrizeFinetune3_17Layer)
      .map(l => l.layerId -> l).toMap
  def layerDefFor(z: Int): LayerDefinition =
    layers.getOrElse(z, LayerDefinition(z, "jpg", downloadedSegmentLayer(_, z)))

  lazy val main =
    encodeResponse { mainRoute }

  val NameR = """(\d+)_(\d+).(?:jpg|png)""".r

  lazy val scrollSegments: Future[Seq[ImageInfo]] =
    for {
      ids <- Future.traverse(ScrollReference.scrolls)(segmentIds)
      infos <- Future.traverse(ids.flatten)(imageInfo)
    } yield infos.flatten.sortBy(i => (i.scrollId, i.segmentId))

  lazy val scrollSegmentsMap: Future[Map[(String, String), SegmentReference]] =
    scrollSegments.map(_.map(i => (i.scrollId, i.segmentId) -> i.ref).toMap)

  def resolveRef(scrollId: String, segmentId: String): Directive1[SegmentReference] =
    scrollSegmentsMap.map(_.get((scrollId, segmentId))).await.orReject

  lazy val mainRoute =
    concat(
      pathSingleSlash {
        get {
          scrollSegments.await { infos =>
            page(html.home(infos))
          }
        }
      },
      path("401") {
        mapResponse(_.withStatus(StatusCodes.Unauthorized)) {
          complete(ToResponseMarshallable(html.error401())(twirlHtmlMarshaller))
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
      pathPrefix("scroll" / Segment / "segment" / Segment) { (scroll, segmentId) =>
        resolveRef(scroll, segmentId) { segment =>
          val isHighResScroll = segment.isHighResSegment

          imageInfo(segment).await.orReject { (info: ImageInfo) =>
            concat(
              userManagement.loggedIn { user =>
                val selectedLayers =
                  if (user.exists(_.admin))
                    if (isHighResScroll) (0 to 150) else (0 to 64)
                  else if (isHighResScroll)
                    if (segment.segmentId == "20231111135340") 0 to 150
                    else
                      (60 to 105 by 3)
                  else
                    (20 to 50 by 2)

                val extraLayers = (if (isHighResScroll) Seq(2350, 2351) else Seq(2344, 2342, 2343)) ++ (
                  if (user.exists(_.admin)) Seq(2400, 2401, 2402, 2403)
                  else Seq.empty
                )
                val allExtraLayers = extraLayers :+ 3000

                // check if inferred layers actually exist
                val allLayerSegments = Future.traverse(allExtraLayers)(l => segmentLayer(segment, l).transform {
                  case Success(f) if f.exists => Success(Some(l))
                  case _                      => Success(None)
                })

                allLayerSegments.await { extras =>
                  val filteredLayers = extras.flatten

                  concat(
                    pathSingleSlash {
                      page(html.segment(info, selectedLayers, filteredLayers))
                    },
                    path("plain") {
                      page(html.segmentPlain(info, selectedLayers, filteredLayers), showDecorations = false)
                    }
                  )
                }
              },
              path("mask") {
                resizedMask(segment).deliver
              },
              pathPrefix("inferred" / Segment) { model =>
                val input: InferenceWorkItemInput = model match {
                  case "youssef-test" if Set("0332", "1667").contains(segment.scrollId) => Youssef_63_32Input
                  case "youssef-test-reversed" if Set("0332", "1667").contains(segment.scrollId) => Youssef_63_32_ReverseInput
                  case "youssef-test" => Youssef_15_32Input
                  case "youssef-test-reversed" => Youssef_15_32_ReverseInput
                  case "grand-prize" => GrandPrize_17_32Input
                  case "grand-prize-finetune0" => GrandPrizeFinetune0_17_32Input
                  case "grand-prize-finetune1" => GrandPrizeFinetune1_17_32Input
                  case "grand-prize-finetune2" => GrandPrizeFinetune2_17_32Input
                  case "grand-prize-finetune3" => GrandPrizeFinetune3_17_32Input
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
              (path("ppm") & withRequestTimeout(10.minutes)) {
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
                      .keepAlive(30.second, () => TextMessage("ping"))
                  )
                } else reject
              },
              pathPrefix(IntNumber) { z =>
                val layerDef = layerDefFor(z)
                concat(
                  path("dzi") {
                    val dziImage = DZIImage(layerDef.extension, 0, 512, info.width, info.height) // FIXME: imagesize
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

  def page(body: Html, showDecorations: Boolean = true): Route =
    userManagement.loggedIn { implicit user =>
      complete(html.page(body, user, showDecorations))
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
        val kvs =
          output.split('\n')
            .map(_.split(": ").map(_.trim))
            .filter(_.size == 2)
            .map(a => a(0) -> a(1)).toMap
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

  def segmentLayer(segment: SegmentReference, layer: Int): Future[File] =
    layerDefFor(layer).layerBase(segment)

  def downloadedSegmentLayer(segment: SegmentReference, layer: Int): Future[File] = {
    import segment._
    val dir = new File(dataDir, s"raw/scroll$scrollId/$segmentId/layers/")
    dir.mkdirs()
    val url = segment.layerUrl(layer)
    val tmpFile = File.createTempFile(".tmp.download", ".tif", dir)
    download(url, tmpFile)
  }

  def resizer(info: ImageInfo, layer: Int, tileX: Int, tileY: Int, z: Int): Future[File] =
    dzdir(info, z).map { dir =>
      val layerDef = layerDefFor(z)
      new File(dir, s"$layer/${tileX}_$tileY.${layerDef.extension}")
    }

  val DZDirCache = LfuCache[(ImageInfo, Int), File]

  def dzdir(info: ImageInfo, z: Int): Future[File] = {
    val layerDef = layerDefFor(z)
    DZDirCache.getOrLoad((info, z), _ => extractDZ(info, z, layerDef.extension))
  }

  def extractDZ(info: ImageInfo, z: Int, suffix: String = ".jpg"): Future[File] = {
    val targetDir = new File(dataDir, s"tiles-dz/scroll${info.scrollId}/${info.segmentId}/layers/${z}_files")
    if (targetDir.exists()) Future.successful(targetDir)
    else segmentLayer(info.ref, z).flatMap { imageFile =>
      targetDir.getParentFile.mkdirs()
      val tmpFile = File.createTempFile(s".tmp.$z", "", targetDir.getParentFile)
      val cmd = s"""vips dzsave $imageFile $tmpFile --suffix .$suffix --tile-size 512 --overlap 0"""

      vipsCommandRunner(cmd).map { _ =>
        val tmpDir = new File(tmpFile.getParentFile, tmpFile.getName + "_files")
        require(tmpDir.renameTo(targetDir))
        if (z < 200) imageFile.delete()
        tmpFile.delete()
        targetDir
      }
    }
  }

  lazy val (vipsCommandRunner, _) = downloadUtils.semaphore[String, String](3) { (cmd, _) =>
    Future {
      import sys.process._
      println(s"Running $cmd")
      cmd.!!
    }(cpuBound)
  }

  def resizedMask(segment: SegmentReference): Future[File] =
    imageInfo(segment).flatMap { info =>
      import segment._
      resizedX(maskFor(segment), new File(dataDir, s"raw/scroll$scrollId/$segmentId/mask_small_autorotated.png"), height = 100, rotate90 = !info.get.isLandscape).map(_.get)
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
      cached(target, negativeTtl = 10.seconds, isValid = f => f0.exists() && f0.lastModified() < f.lastModified()) { () =>
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
      segment.maskUrl,
      new File(dataDir, s"raw/scroll$scrollId/$segmentId/mask.png"))
  }

  val AlphaMaskCache = computeCache[SegmentReference](
    segment => new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/mask-alpha.png")
  ) { segment =>
      maskFor(segment).flatMap { mask =>
        val target = new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/mask-alpha.png")
        vipsCommandRunner(s"""vips bandjoin "${mask.getAbsolutePath} ${mask.getAbsolutePath}" ${target.getAbsolutePath}""")
          .map(_ => target)
      }
    }

  def alphaMaskFor(segment: SegmentReference): Future[File] =
    AlphaMaskCache(segment)

  def inklabelFor(segment: SegmentReference): Future[Option[File]] = {
    import segment._
    cacheDownload(
      segment.inklabelUrl,
      new File(dataDir, s"inklabels/scroll$scrollId/$segmentId/inklabel.png")
    )
      .map(f => if (f.exists()) Some(f) else None)
      .transform(x => Success(x.toOption.flatten))
  }

  def areaFor(segment: SegmentReference): Future[Option[Float]] = {
    import segment._
    cacheDownload(
      s"${segment.baseUrl}area_cm2.txt",
      new File(dataDir, s"raw/scroll$scrollId/$segmentId/area_cm2.txt")
    )
      .map(f => scala.io.Source.fromFile(f).getLines().next().toFloat)
      .transform(x => Success(x.toOption))
  }

  def segmentIds(scroll: ScrollReference): Future[Seq[SegmentReference]] =
    segmentIds(scroll.baseUrl, new File(config.dataDir, s"raw/scroll${scroll.scrollId}/${scroll.base}-path-listing.html"))
      .map(_.map(segment => SegmentReference(scroll, segment)))

  val LinkR = """.*href="(.*)/".*""".r
  def segmentIds(baseUrl: String, targetFile: File): Future[Seq[String]] =
    cacheDownload(baseUrl, targetFile, ttl = 1.hour).map { f =>
      scala.io.Source.fromFile(f).getLines().collect {
        case LinkR(segmentId) if !segmentId.startsWith("..") =>
          segmentId
      }.toVector
    }

  def targetFileForInput(segment: SegmentReference, input: WorkItemInput): File = {
    import segment._
    input match {
      case InferenceWorkItemInput(model, startLayer, stride, reverseLayers) =>
        val reversed = if (reverseLayers) "_reverse" else ""
        new File(dataDir, s"inferred/scroll$scrollId/$segmentId/inference_${model}_${startLayer}_${stride}$reversed.png")
      case PPMFingerprintWorkItemInput =>
        new File(dataDir, s"ppm/scroll${segment.scrollId}/${segment.segmentId}/fingerprint.json")
      case DownsamplePPMWorkItemInput(tpe, downsamplingBits) =>
        new File(dataDir, s"ppm/scroll${segment.scrollId}/${segment.segmentId}/uvmap-${tpe}-${downsamplingBits}.bin")
    }
  }

  val DownSampleU16_2Input = DownsamplePPMWorkItemInput("u16", 2)
  val Youssef_15_32Input = InferenceWorkItemInput("youssef-test", 15, 32, false)
  val GrandPrize_17_32Input = InferenceWorkItemInput("grand-prize", 17, 32, false)
  val GrandPrizeFinetune0_17_32Input = InferenceWorkItemInput("grand-prize-finetune0", 17, 32, false)
  val GrandPrizeFinetune1_17_32Input = InferenceWorkItemInput("grand-prize-finetune1", 17, 32, false)
  val GrandPrizeFinetune2_17_32Input = InferenceWorkItemInput("grand-prize-finetune2", 17, 32, false)
  val GrandPrizeFinetune3_17_32Input = InferenceWorkItemInput("grand-prize-finetune3", 17, 32, false)
  val Youssef_15_32_ReverseInput = InferenceWorkItemInput("youssef-test", 15, 32, true)
  val Youssef_63_32Input = InferenceWorkItemInput("youssef-test", 63, 32, false)
  val Youssef_63_32_ReverseInput = InferenceWorkItemInput("youssef-test", 63, 32, true)

  type Filter = ImageInfo => Boolean
  lazy val requestedWorkInputs: Seq[(WorkItemInput, Filter)] =
    Seq(
      Youssef_15_32Input -> (s => s.scrollId == "1" && s.area.exists(_ > 10) && !s.ref.isHighResSegment /*|| s.scrollId == "2"*/ ),
      GrandPrize_17_32Input -> (s => s.area.exists(_ > 10) && !s.ref.isHighResSegment), //(s => s.scrollId == "1"),
      //GrandPrizeFinetune0_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10)),
      GrandPrizeFinetune1_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10) && !s.ref.isHighResSegment),
      //GrandPrizeFinetune2_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10)),
      GrandPrizeFinetune3_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10) && !s.ref.isHighResSegment),
      Youssef_15_32_ReverseInput -> (s => s.scrollId == "1" /*|| s.scrollId == "2"*/ ),
      //Youssef_63_32Input -> (s => s.scrollId == "332" || s.scrollId == "1667"),
      //Youssef_63_32_ReverseInput -> (s => s.scrollId == "332" || s.scrollId == "1667"),
      PPMFingerprintWorkItemInput -> (_.scrollId == "1"),
      DownSampleU16_2Input -> (_ => true),
    )

  val runnerId = System.currentTimeMillis().toString
  lazy val workItems: Future[Seq[WorkItem]] =
    Source.futureSource(scrollSegments.map(x => Source(x.sortBy(_.segmentId).reverse)))
      .mapConcat { info =>
        requestedWorkInputs
          .filter(_._2(info))
          .map(input => info.ref -> input._1)
      }
      .filter { case (ref, input) => !targetFileForInput(ref, input).exists() }
      .zipWithIndex
      .map { case ((ref, input), id) => WorkItem(s"$runnerId-$id", ref, input.`type`, input) }
      .runWith(Sink.seq)

  lazy val workItemManager: Future[WorkItemManager] =
    for {
      workItems <- workItems
      scrollSegments <- scrollSegments
    } yield WorkItemManager(workItems, scrollSegments.map(_.ref))

  lazy val depsFile: Directive1[File] =
    provide(sys.props("java.class.path").split(":").find(_.contains("deps.jar")).map(new File(_))).orReject
}
