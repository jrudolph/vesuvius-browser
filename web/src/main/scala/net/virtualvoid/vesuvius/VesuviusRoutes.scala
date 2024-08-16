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
import scala.util.Try

trait LayerSource {
  def layerFor(segment: SegmentReference): Future[Option[File]]
}

trait InferenceLayerSource extends LayerSource {
  def input: InferenceWorkItemInput
}

case class LayerDefinition(
    name:      String,
    extension: String,
    source:    LayerSource,
    isPublic:  Boolean
) {
  def input: InferenceWorkItemInput = source.asInstanceOf[InferenceLayerSource].input

  def layerFor(segment: SegmentReference): Future[Option[File]] =
    source.layerFor(segment)

  def longName: String =
    source match {
      case s: InferenceLayerSource =>
        val reverse = if (s.input.parameters.reverseLayers) " (reversed)" else ""
        s.input.modelCheckpoint.name + reverse
      case _ => name
    }
  def url: String =
    source match {
      case s: InferenceLayerSource => s.input.modelCheckpoint.architecture.url
      case _                       => ""
    }
}

class VesuviusRoutes(config: AppConfig)(implicit system: ActorSystem) extends Directives with TwirlSupport with SprayJsonSupport {

  import system.dispatcher
  import config.dataDir

  val cpuBound = system.dispatchers.lookup("cpu-bound-dispatcher")

  val userManagement = UserManagement(config)
  val downloadUtils = new DownloadUtils(config)
  import downloadUtils._
  val volumeMetadataRepository = VolumeMetadataRepository(downloadUtils, dataDir)

  case class InferenceLayerSourceImpl(
      input: InferenceWorkItemInput
  ) extends InferenceLayerSource {
    def layerFor(segment: SegmentReference): Future[Option[File]] =
      Future.successful(Some(targetFileForInput(segment, input)).filter(_.exists))
  }
  case class SegmentLayerSource(
      layer: Int
  ) extends LayerSource {
    def layerFor(segment: SegmentReference): Future[Option[File]] =
      downloadedSegmentLayer(segment, layer).transform(r => Success(r.toOption))
  }
  case class AnonymousSource(f: SegmentReference => Future[Option[File]]) extends LayerSource {
    def layerFor(segment: SegmentReference): Future[Option[File]] = f(segment)
  }

  def inferenceLayer(input: InferenceWorkItemInput, isPublic: Boolean): LayerDefinition = {
    val params = input.parameters
    LayerDefinition(s"${input.modelCheckpoint.shortName}_${params.suffix}", "jpg", InferenceLayerSourceImpl(input), isPublic)
  }

  val PolytropeTest3Predictions = {
    def dir(segment: SegmentReference) = new File(dataDir, s"external/polytrope-test3-model/Scroll${segment.scrollId}/")

    def findFile(segment: SegmentReference): Future[Option[File]] = {
      val res = FileUtils.firstFileNameMatching(dir(segment), s""".*${segment.segmentId}.*\\.png""")
      Future.successful(res)
    }

    LayerDefinition("polytrope-test3-predictions", "jpg", AnonymousSource(findFile), isPublic = true)
  }

  val InkLabelLayer = LayerDefinition("inklabel", "jpg", AnonymousSource(inklabelFor(_)), isPublic = true)
  val AlphaMaskLayer = LayerDefinition("alpha", "png", AnonymousSource(s => alphaMaskFor(s).map(Some(_))), isPublic = false)

  lazy val allLayers =
    Seq(
      inferenceLayer(GrandPrize_17_32Input, isPublic = true),
      PolytropeTest3Predictions,
      inferenceLayer(GrandPrizeFinetune0_17_32Input, isPublic = false),
      inferenceLayer(GrandPrizeFinetune1_17_32Input, isPublic = false),
      inferenceLayer(GrandPrizeFinetune2_17_32Input, isPublic = false),
      inferenceLayer(GrandPrizeFinetune3_17_32Input, isPublic = false),
      inferenceLayer(Youssef_15_32Input, isPublic = true),
      inferenceLayer(Youssef_15_32_ReverseInput, isPublic = true),
      inferenceLayer(Youssef_63_32Input, isPublic = true),
      inferenceLayer(Youssef_63_32_ReverseInput, isPublic = true),
      InkLabelLayer,
      AlphaMaskLayer
    )
  lazy val allLayersMap =
    allLayers.map(l => l.name -> l).toMap

  lazy val (PublicLayers @ _, AdminLayers @ _) = allLayers.partition(_.isPublic)

  def layerDefFor(name: String): Option[LayerDefinition] =
    allLayersMap
      .get(name)
      .orElse(Try(name.toInt).toOption.map(z => LayerDefinition(name, "jpg", SegmentLayerSource(z), isPublic = true)))

  lazy val MainScreenLayerThumbnails = Seq(
    "grand-prize_17_32",
    "polytrope-test3-predictions",
    "first-word_15_32",
    "first-word_15_32_reverse",
  ).map(layerDefFor(_).get)

  lazy val AdminMainScreenLayerThumbnails = Seq(
    "grand-prize-finetune0_17_32",
    "grand-prize-finetune1_17_32",
    "grand-prize-finetune2_17_32",
    "grand-prize-finetune3_17_32",
  ).map(layerDefFor(_).get) ++ MainScreenLayerThumbnails

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
            userManagement.loggedIn { user =>
              val thumbs = if (user.exists(_.admin)) AdminMainScreenLayerThumbnails else MainScreenLayerThumbnails
              page(html.home(infos, thumbs))
            }
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
              segmentPath { isPlain =>
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

                  val extraLayers = PublicLayers ++ (
                    if (user.exists(_.admin)) AdminLayers
                    else Seq.empty
                  )

                  // check if inferred layers actually exist
                  val allLayerSegments = Future.traverse(extraLayers)(l => l.source.layerFor(segment).transform {
                    case Success(Some(f)) if f.exists => Success(Some(l))
                    case _                            => Success(None)
                  })

                  allLayerSegments.await { extras =>
                    val filteredLayers = extras.flatten

                    if (isPlain) {
                      page(html.segmentPlain(info, selectedLayers, filteredLayers), showDecorations = false)
                    } else {
                      page(html.segment(info, selectedLayers, filteredLayers))
                    }
                  }
                }
              },
              path("mask") {
                resizedMask(segment).deliver
              },
              pathPrefix("inferred" / Segment) { layer =>
                val layerDef = layerDefFor(layer).get

                concat(
                  pathEnd {
                    concat(
                      resizedLayer(segment, layerDef).await.orReject.deliver,
                      complete(ImageTools.EmptyImageResponse)
                    )
                  },
                  path("full") {
                    parameter("show".?) { show =>
                      val inner = layerDef.source.layerFor(segment).await.orReject.deliver
                      if (show.isDefined) inner
                      else
                        respondWithHeader(headers.`Content-Disposition`(headers.ContentDispositionTypes.attachment, Map("filename" -> s"${segment.segmentId}_inference_${layer}.png"))) {
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
                        case _ => throw new IllegalStateException
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
              pathPrefix(Segment.flatMap(layerDefFor)) { layerDef =>
                concat(
                  path("dzi") {
                    val dziImage = DZIImage(layerDef.extension, 0, 512, info.width, info.height) // FIXME: imagesize
                    complete(JsObject("Image" -> dziImage.toJson))
                  },
                  path("dzi_files" / IntNumber / Segment) { (layer, name) =>
                    val NameR(xStr, yStr) = name: @unchecked
                    val x = xStr.toInt
                    val y = yStr.toInt

                    resizer(info, layer, x, y, layerDef.name).deliver
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

  def segmentPath: Directive1[Boolean] =
    (pathSingleSlash & provide(false)) |
      (path("plain") & provide(true))

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
      meta <- SegmentMetadataCache(segment).transform(x => Success(x.toOption))
      volumeMetadata <- meta.map(m => volumeMetadataRepository.metadataForVolume(segment.scrollRef, m.volume).transform(x => Success(x.toOption))).getOrElse(Future.successful(None))
    } yield ImageInfo(segment, width, height, area, meta, volumeMetadata)
  }.transform(x => Success(x.toOption))

  def segmentLayer(segment: SegmentReference, layerName: String): Future[File] =
    layerDefFor(layerName)
      .map(_.layerFor(segment).map(_.get))
      .getOrElse(Future.failed(new RuntimeException(s"Layer $layerName not found")))

  def downloadedSegmentLayer(segment: SegmentReference, layer: Int): Future[File] = {
    import segment._
    val dir = new File(dataDir, s"raw/scroll$scrollId/$segmentId/layers/")
    dir.mkdirs()
    val url = segment.layerUrl(layer)
    val tmpFile = File.createTempFile(".tmp.download", ".tif", dir)
    download(url, tmpFile)
  }

  def resizer(info: ImageInfo, layer: Int, tileX: Int, tileY: Int, layerName: String): Future[File] =
    dzdir(info, layerName).map { dir =>
      val layerDef = layerDefFor(layerName).get
      new File(dir, s"$layer/${tileX}_$tileY.${layerDef.extension}")
    }

  val DZDirCache = LfuCache[(ImageInfo, String), File]

  def dzdir(info: ImageInfo, layerName: String): Future[File] = {
    val layerDef = layerDefFor(layerName).get
    DZDirCache.getOrLoad((info, layerName), _ => extractDZ(info, layerName, layerDef.extension))
  }

  def extractDZ(info: ImageInfo, layerName: String, suffix: String = ".jpg"): Future[File] = {
    val targetDir = new File(dataDir, s"tiles-dz/scroll${info.scrollId}/${info.segmentId}/layers/${layerName}_files")
    if (targetDir.exists()) Future.successful(targetDir)
    else segmentLayer(info.ref, layerName).flatMap { imageFile =>
      targetDir.getParentFile.mkdirs()
      val tmpFile = File.createTempFile(s".tmp.$layerName", "", targetDir.getParentFile)
      val cmd = s"""vips dzsave $imageFile $tmpFile --suffix .$suffix --tile-size 512 --overlap 0"""

      vipsCommandRunner(cmd).map { _ =>
        val tmpDir = new File(tmpFile.getParentFile, tmpFile.getName + "_files")
        require(tmpDir.renameTo(targetDir))
        if (Try(layerName.toInt).isSuccess) imageFile.delete()
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
      resizedLetterbox(maskFor(segment), new File(dataDir, s"raw/scroll$scrollId/$segmentId/mask_300x150-black-letterbox.png"), width = 300, height = 150).map(_.get)
    }

  def resizedLayer(segment: SegmentReference, layer: LayerDefinition): Future[Option[File]] =
    for {
      i <- imageInfo(segment)
      file <- layer.layerFor(segment)
      resized <- file.map(file => resizedLetterbox(
        Future.successful(file),
        new File(thumbnailDir(file), file.getName.dropRight(4) + "_small_300x150-black-letterbox.png"), width = 300, height = 150))
        .getOrElse(Future.successful(None))
    } yield resized

  def thumbnailDir(orig: File): File = {
    require(orig.getAbsolutePath.startsWith(dataDir.getAbsolutePath))
    val relativePathToData = orig.getAbsolutePath.drop(dataDir.getAbsolutePath.size)
    new File(dataDir, s"thumbnails$relativePathToData").getParentFile()
  }

  def resizedLetterbox(orig: Future[File], target: File, width: Int, height: Int): Future[Option[File]] =
    orig.flatMap { f0 =>
      cached(target, negativeTtl = 10.seconds, isValid = f => f0.exists() && f0.lastModified() < f.lastModified()) { () =>
        Option(f0).filter(_.exists).getOrElse(throw new RuntimeException(s"File $f0 does not exist"))
        import sys.process._

        val tmpFile = File.createTempFile(".tmp.resized", ".png", target.getParentFile)
        val cmd = s"""vips thumbnail ${f0.getAbsolutePath} $tmpFile ${width} --height $height"""
        println(cmd)
        cmd.!!
        val tmpFile2 = File.createTempFile(".tmp.resized", ".png", target.getParentFile)
        val cmd2 = s"""vips gravity ${tmpFile.getAbsolutePath} ${tmpFile2.getAbsolutePath} centre $width $height --background "0,0,0""""
        cmd2.!!
        tmpFile.delete()

        require(tmpFile2.renameTo(target))
        Future.successful(target)
      }
    }.transform(x => Success(x.toOption))

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
      new File(dataDir, s"inklabels/scroll$scrollId/$segmentId/inklabel.png"),
      negativeTtl = 30.days
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

  val SegmentMetadataCache: Cache[SegmentReference, SegmentMetadata] =
    downloadUtils.downloadCache[SegmentReference](_.metaUrl, segment => new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/meta.json"))
      .map((_, metadata) => scala.io.Source.fromFile(metadata).mkString.parseJson.convertTo[SegmentMetadata])

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
      case InferenceWorkItemInput(checkpoint, params) =>
        val shortName0 = checkpoint.shortName
        val shortName = if (checkpoint.architecture == InferenceModelArchitecture.FirstWordModel) "youssef-test" else shortName0
        new File(dataDir, s"inferred/scroll$scrollId/$segmentId/inference_${shortName}_${params.suffix}.png")
      case PPMFingerprintWorkItemInput =>
        new File(dataDir, s"ppm/scroll${segment.scrollId}/${segment.segmentId}/fingerprint.json")
      case DownsamplePPMWorkItemInput(tpe, downsamplingBits) =>
        new File(dataDir, s"ppm/scroll${segment.scrollId}/${segment.segmentId}/uvmap-${tpe}-${downsamplingBits}.bin")
    }
  }

  val Forward15Stride32 = InferenceParameters(15, 32, reverseLayers = false)
  val Reverse15Stride32 = InferenceParameters(15, 32, reverseLayers = true)

  val Forward63Stride32 = InferenceParameters(63, 32, reverseLayers = false)
  val Reverse63Stride32 = InferenceParameters(63, 32, reverseLayers = true)

  val Forward17Stride32 = InferenceParameters(17, 32, reverseLayers = false)

  import InferenceModelCheckpoint._
  val DownSampleU16_2Input = DownsamplePPMWorkItemInput("u16", 2)

  val GrandPrize_17_32Input = InferenceWorkItemInput(GrandPrizeModel, Forward17Stride32)
  val GrandPrizeFinetune0_17_32Input = InferenceWorkItemInput(GrandPrizeFineTune0, Forward17Stride32)
  val GrandPrizeFinetune1_17_32Input = InferenceWorkItemInput(GrandPrizeFineTune1, Forward17Stride32)
  val GrandPrizeFinetune2_17_32Input = InferenceWorkItemInput(GrandPrizeFineTune2, Forward17Stride32)
  val GrandPrizeFinetune3_17_32Input = InferenceWorkItemInput(GrandPrizeFineTune3, Forward17Stride32)

  val Youssef_15_32Input = InferenceWorkItemInput(FirstWordModel, Forward15Stride32)
  val Youssef_15_32_ReverseInput = InferenceWorkItemInput(FirstWordModel, Reverse15Stride32)

  val Youssef_63_32Input = InferenceWorkItemInput(FirstWordModel, Forward63Stride32)
  val Youssef_63_32_ReverseInput = InferenceWorkItemInput(FirstWordModel, Forward63Stride32)

  def hasReasonableSize(info: ImageInfo): Boolean =
    info.area.exists(_ > 8) || (info.width * info.height > 100 * 1000 * 1000)

  type Filter = ImageInfo => Boolean
  lazy val requestedWorkInputs: Seq[(WorkItemInput, Filter)] =
    Seq(
      Youssef_15_32Input -> (s => s.scrollId == "1" && hasReasonableSize(s) && !s.ref.isHighResSegment /*|| s.scrollId == "2"*/ ),
      GrandPrize_17_32Input -> (s => hasReasonableSize(s) && !s.ref.isHighResSegment), //(s => s.scrollId == "1"),
      //GrandPrizeFinetune0_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10)),
      GrandPrizeFinetune1_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && hasReasonableSize(s) && !s.ref.isHighResSegment),
      //GrandPrizeFinetune2_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10)),
      GrandPrizeFinetune3_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && hasReasonableSize(s) && !s.ref.isHighResSegment),
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
