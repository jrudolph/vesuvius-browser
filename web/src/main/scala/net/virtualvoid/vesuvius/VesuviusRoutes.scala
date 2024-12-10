package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, ToResponseMarshallable}
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage}
import org.apache.pekko.http.scaladsl.model.{StatusCodes, headers}
import org.apache.pekko.http.scaladsl.server.{Directive1, Directives, Route}
import org.apache.pekko.stream.scaladsl.{FileIO, Flow, Sink, Source}
import play.twirl.api.Html
import spray.json.*

import java.io.File
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.Success
import scala.util.Try
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.model.MediaTypes
import org.apache.pekko.http.scaladsl.model.HttpCharsets

extension (f: Future[Boolean]) {
  def ||(other: => Future[Boolean])(implicit ec: ExecutionContext): Future[Boolean] =
    f.flatMap {
      case true  => Future.successful(true)
      case false => other
    }
}

trait LayerSource {
  def layerFor(segment: SegmentReference): Future[Option[File]]
  /* Returns the file that would be used for the layer, even if it doesn't exist */
  def layerFileFor(segment: SegmentReference): File

  def layerExists(segment: SegmentReference): Future[Boolean]
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

  /* Returns the file that would be used for the layer, even if it doesn't exist */
  def layerFileFor(segment: SegmentReference): File =
    source.layerFileFor(segment)

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

class VesuviusRoutes(val config: AppConfig)(implicit val system: ActorSystem) extends VesuviusApi with TwirlSupport with SprayJsonSupport {
  import Directives._
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

    def layerFileFor(segment: SegmentReference): File = targetFileForInput(segment, input)

    override def layerExists(segment: SegmentReference): Future[Boolean] =
      Future.successful(targetFileForInput(segment, input).exists)
  }
  case class SegmentLayerSource(
      layer: Int
  ) extends LayerSource {
    def layerFor(segment: SegmentReference): Future[Option[File]] =
      downloadedSegmentLayer(segment, layer).transform(r => Success(r.toOption))

    def layerFileFor(segment: SegmentReference): File =
      segmentLayerTarget(segment, layer)

    def layerExists(segment: SegmentReference): Future[Boolean] =
      Future.successful(layerFileFor(segment).exists) ||
        downloadUtils.urlExists(segment.layerUrl(layer))
  }
  case class AnonymousSource(f: SegmentReference => Future[Option[File]], targetFileFor: SegmentReference => File) extends LayerSource {
    def layerFor(segment: SegmentReference): Future[Option[File]] = f(segment)
    def layerFileFor(segment: SegmentReference): File = targetFileFor(segment)

    def layerExists(segment: SegmentReference): Future[Boolean] =
      Future.successful(targetFileFor(segment).exists)
  }

  case class FileCacheSource(cache: FileCache[SegmentReference], url: SegmentReference => String) extends LayerSource {
    def layerFor(segment: SegmentReference): Future[Option[File]] = cache(segment).transform(x => Success(x.toOption))
    def layerFileFor(segment: SegmentReference): File = cache.targetFile(segment)

    def layerExists(segment: SegmentReference): Future[Boolean] =
      Future.successful(cache.targetFile(segment).exists) || downloadUtils.urlExists(url(segment))
  }

  def inferenceLayer(input: InferenceWorkItemInput, isPublic: Boolean): LayerDefinition = {
    val params = input.parameters
    LayerDefinition(s"${input.modelCheckpoint.shortName}_${params.suffix}", "jpg", InferenceLayerSourceImpl(input), isPublic)
  }

  def externalLayer(name: String, baseDir: String, extension: String = "png", isPublic: Boolean = true): LayerDefinition = {
    def dir(segment: SegmentReference) = new File(dataDir, s"$baseDir/Scroll${segment.scrollId}/")
    def simpleFile(segment: SegmentReference) = new File(dir(segment), s"${segment.segmentId}.${extension}")

    def findFile(segment: SegmentReference): Option[File] = {
      val res = FileUtils.firstFileNameMatching(dir(segment), s""".*${segment.segmentId}.*\\.${extension}""")
      res
    }

    LayerDefinition(name, "jpg", AnonymousSource(findFile.andThen(Future.successful), segment => simpleFile(segment)), isPublic)
  }

  val PolytropeTest1Predictions = externalLayer("polytrope-test1-predictions", "external/polytrope-test1-model", isPublic = false)
  val PolytropeTest2Predictions = externalLayer("polytrope-test2-predictions", "external/polytrope-test2-model", isPublic = false)
  val PolytropeTest3Predictions = externalLayer("polytrope-test3-predictions", "external/polytrope-test3-model")
  val PolytropeInklabels20240816 = externalLayer("polytrope-inklabels-2024-08-16", "external/polytrope-inklabels-2024-08-16")
  val RepushkoInklabels20231123 = externalLayer("repushko-inklabels-2023-11-23", "external/repushko-inklabels-2023-11-23", isPublic = false)
  val GrandPrizeInklabels = externalLayer("grand-prize-inklabels", "external/grand-prize-inklabels")
  val FirstLettersInklabels = externalLayer("first-letters-inklabels", "external/first-letters-inklabels")

  lazy val AutosegmentedPrediction =
    LayerDefinition("autosegmented-prediction", "jpg", FileCacheSource(AutoSegmentPredictionCache, AutoSegmentedDirectoryStyle.predictionUrlFor), isPublic = true)

  lazy val InkLabelLayer = LayerDefinition("inklabel", "jpg", FileCacheSource(InklabelCache, _.inklabelUrl), isPublic = true)
  lazy val AlphaMaskLayer = LayerDefinition("alpha", "png", FileCacheSource(AlphaMaskCache, _.maskUrl), isPublic = false)

  lazy val allLayers =
    Seq(
      inferenceLayer(GrandPrize_17_32Input, isPublic = true),
      PolytropeTest1Predictions,
      PolytropeTest2Predictions,
      PolytropeTest3Predictions,
      inferenceLayer(GrandPrizeFinetune0_17_32Input, isPublic = false),
      inferenceLayer(GrandPrizeFinetune1_17_32Input, isPublic = false),
      inferenceLayer(GrandPrizeFinetune2_17_32Input, isPublic = false),
      inferenceLayer(GrandPrizeFinetune3_17_32Input, isPublic = false),
      inferenceLayer(TimesformerScroll5_27112024_17_32Input, isPublic = true),
      inferenceLayer(Youssef_15_32Input, isPublic = true),
      inferenceLayer(Youssef_15_32_ReverseInput, isPublic = true),
      inferenceLayer(Youssef_63_32Input, isPublic = true),
      inferenceLayer(Youssef_63_32_ReverseInput, isPublic = true),
      GrandPrizeInklabels,
      PolytropeInklabels20240816,
      FirstLettersInklabels,
      RepushkoInklabels20231123,
      AutosegmentedPrediction,
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

  private lazy val LayersForCache: Future[Map[SegmentReference, Seq[String]]] =
    scrollSegments.flatMap { segments =>
      Future.traverse(segments)(s => retrieveLayersFor(s.ref).map(s.ref -> _))
        .map(_.toMap)
    }

  def layersFor(segment: SegmentReference): Future[Seq[String]] =
    LayersForCache.map(_.getOrElse(segment, Seq.empty))

  private def retrieveLayersFor(segment: SegmentReference): Future[Seq[String]] =
    Future.traverse(PublicLayers)(l => l.source.layerExists(segment).map {
      case true  => Seq(l.name)
      case false => Seq.empty
    }).map(_.flatten).flatMap { layers =>
      downloadUtils.urlExists(segment.maskUrl).map {
        case true  => "mask" +: layers
        case false => layers
      }
    }

  lazy val MainScreenLayerThumbnails = Seq(
    "grand-prize_17_32",
    "timesformer-scroll5-27112024_17_32",
    "polytrope-test3-predictions",
    "first-word_15_32",
    "first-word_15_32_reverse",
    "grand-prize-inklabels",
    "polytrope-inklabels-2024-08-16",
    "first-letters-inklabels",
    "autosegmented-prediction",
  ).map(layerDefFor(_).get)

  lazy val AdminMainScreenLayerThumbnails = Seq(
    "grand-prize-finetune0_17_32",
    "grand-prize-finetune1_17_32",
    "grand-prize-finetune2_17_32",
    "grand-prize-finetune3_17_32",
    "polytrope-test1-predictions",
    "polytrope-test2-predictions",
    "repushko-inklabels-2023-11-23"
  ).map(layerDefFor(_).get) ++ MainScreenLayerThumbnails

  lazy val main =
    encodeResponse { mainRoute }

  val NameR = """(\d+)_(\d+).(?:jpg|png)""".r

  lazy val scrollSegments: Future[Seq[SegmentInfo]] =
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

          imageInfo(segment).await.orReject { (info: SegmentInfo) =>
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
                parameter("width".as[Int].?(config.thumbnailWidth), "height".as[Int].?(config.thumbnailHeight), "ext".as[String].?(config.thumbnailExtension)) { (width, height, ext) =>
                  resizedMask(segment, width, height, ext).deliver
                }
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
      pathPrefix("api")(apiRoutes),
      path("css" / "settings.css") {
        complete(HttpEntity(MediaTypes.`text/css`.toContentType(HttpCharsets.`UTF-8`), html.settings(config).body))
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

  val InfoCache = LfuCache[SegmentReference, Option[SegmentInfo]]

  def imageInfo(segment: SegmentReference): Future[Option[SegmentInfo]] =
    InfoCache.getOrLoad(segment, _ => _imageInfo(segment))

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

  def _imageInfo(segment: SegmentReference): Future[Option[SegmentInfo]] = {
    for {
      (width, height) <- sizeOf(segment)
      area <- areaFor(segment)
      meta <- SegmentMetadataCache(segment).transform(x => Success(x.toOption))
      volumeId = meta.map(_.volume).orElse(Option(segment.scrollRef.defaultVolumeId).filter(_.nonEmpty))
      volumeMetadata <- volumeId
        .map(volumeId =>
          volumeMetadataRepository.metadataForVolume(segment.scrollRef, volumeId)
            .transform(x => Success(x.toOption)))
        .getOrElse(Future.successful(None))
    } yield SegmentInfo(segment, width, height, area, meta, volumeMetadata)
  }.transform { x =>
    if (x.isFailure) {
      println(s"Failed to get image info for $segment: ${x.failed.get}")
    }
    Success(x.toOption)
  }

  def segmentLayer(segment: SegmentReference, layerName: String): Future[File] =
    layerDefFor(layerName)
      .map(_.layerFor(segment).map(_.get))
      .getOrElse(Future.failed(new RuntimeException(s"Layer $layerName not found")))

  def segmentLayerTarget(segment: SegmentReference, layer: Int): File =
    new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/layers/$layer.${segment.layerFileExtension}")

  def downloadedSegmentLayer(segment: SegmentReference, layer: Int): Future[File] = {
    import segment._
    val url = segment.layerUrl(layer)
    val target = segmentLayerTarget(segment, layer)
    target.getParentFile().mkdirs()
    download(url, target)
  }

  lazy val AutoSegmentPredictionCache = downloadUtils.downloadCache[SegmentReference](
    AutoSegmentedDirectoryStyle.predictionUrlFor,
    segment => new File(dataDir, s"raw/autosegment/scroll${segment.scrollId}/${segment.segmentId}/prediction.png"),
    maxConcurrentRequests = 3
  )

  def resizer(info: SegmentInfo, layer: Int, tileX: Int, tileY: Int, layerName: String): Future[File] =
    dzdir(info, layerName).map { dir =>
      val layerDef = layerDefFor(layerName).get
      new File(dir, s"$layer/${tileX}_$tileY.${layerDef.extension}")
    }

  val DZDirCache = LfuCache[(SegmentInfo, String), File]

  def dzdir(info: SegmentInfo, layerName: String): Future[File] = {
    val layerDef = layerDefFor(layerName).get
    DZDirCache.getOrLoad((info, layerName), _ => extractDZ(info, layerName, layerDef.extension))
  }

  def extractDZ(info: SegmentInfo, layerName: String, suffix: String = ".jpg"): Future[File] = {
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

  lazy val (vipsCommandRunner, _) = downloadUtils.semaphore[String, String](config.concurrentResizes) { (cmd, _) =>
    Future {
      import sys.process._
      println(s"Running $cmd")
      cmd.!!
    }(cpuBound)
  }

  def resizedMask(segment: SegmentReference, width: Int = config.thumbnailWidth, height: Int = config.thumbnailHeight, extension: String = config.thumbnailExtension): Future[File] =
    for {
      mask <- maskFor(segment)
      resized <- ThumbnailCache((mask, width, height, extension, MaskFileAcquire(segment)))
    } yield resized

  def resizedLayer(segment: SegmentReference, layer: LayerDefinition, extension: String = config.thumbnailExtension): Future[Option[File]] =
    for {
      i <- imageInfo(segment)
      file = layer.layerFileFor(segment)
      resized <- ThumbnailCache((file, config.thumbnailWidth, config.thumbnailHeight, extension, LayerAcquire(segment, layer))).transform(x => Success(x.toOption))
    } yield resized

  sealed trait BaseFileAcquire {
    def get(): Future[File]
  }
  case class LayerAcquire(segment: SegmentReference, layer: LayerDefinition) extends BaseFileAcquire {
    def get(): Future[File] = layer.layerFor(segment).map(_.get)
  }
  case class MaskFileAcquire(segment: SegmentReference) extends BaseFileAcquire {
    def get(): Future[File] = maskFor(segment)
  }

  lazy val ThumbnailCache = {
    def thumbnailDir(orig: File): File = {
      require(orig.getAbsolutePath.startsWith(dataDir.getAbsolutePath))
      val relativePathToData = orig.getAbsolutePath.drop(dataDir.getAbsolutePath.size)
      new File(dataDir, s"thumbnails$relativePathToData").getParentFile()
    }

    def thumbnailLocationFor(f: File, w: Int, h: Int, extension: String): File =
      new File(thumbnailDir(f), f.getName.dropRight(4) + s"_small_${w}x${h}-black-letterbox.$extension")

    downloadUtils.computeCache[(File, Int, Int, String, BaseFileAcquire)](
      { case (f, w, h, ext, _) => thumbnailLocationFor(f, w, h, ext) }) {
        case (f, w, h, ext, acquire) =>
          if (f.exists())
            createLetterboxThumbnail(f, thumbnailLocationFor(f, w, h, ext), w, h, ext)
          else
            acquire.get().flatMap { f =>
              createLetterboxThumbnail(f, thumbnailLocationFor(f, w, h, ext), w, h, ext)
            }
      }
  }

  def createLetterboxThumbnail(f0: File, target: File, width: Int, height: Int, extension: String): Future[File] = Future(()).flatMap { _ =>
    Option(f0).filter(_.exists).getOrElse({ println(s"File $f0 does not exist"); throw new RuntimeException(s"File $f0 does not exist") })
    val tmpFile = File.createTempFile(".tmp.resized", ".png", target.getParentFile)
    val cmd = s"""vips thumbnail ${f0.getAbsolutePath} $tmpFile ${width} --height $height"""
    println(cmd)
    for {
      _ <- vipsCommandRunner(cmd)
      tmpFile2 = File.createTempFile(".tmp.resized", s".$extension", target.getParentFile)
      cmd2 = s"""vips gravity ${tmpFile.getAbsolutePath} ${tmpFile2.getAbsolutePath} centre $width $height --background "0,0,0""""
      _ <- vipsCommandRunner(cmd2)
    } yield {
      tmpFile.delete()
      require(tmpFile2.renameTo(target))
      target
    }
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

  lazy val MaskCache = downloadUtils.downloadCache[SegmentReference](
    _.maskUrl,
    segment => new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/mask.png")
  )

  def maskFor(segment: SegmentReference): Future[File] = MaskCache(segment)

  lazy val AlphaMaskCache = computeCache[SegmentReference](
    segment => new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/mask-alpha.png")
  ) { segment =>
      maskFor(segment).flatMap { mask =>
        val target = new File(dataDir, s"raw/scroll${segment.scrollId}/${segment.segmentId}/mask-alpha.png")
        vipsCommandRunner(s"""vips bandjoin "${mask.getAbsolutePath} ${mask.getAbsolutePath}" ${target.getAbsolutePath}""")
          .map(_ => target)
      }
    }

  lazy val InklabelCache =
    downloadUtils.downloadCache[SegmentReference](
      _.inklabelUrl,
      segment => { import segment._; new File(dataDir, s"inklabels/scroll$scrollId/$segmentId/inklabel.png") },
    )

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
    Future.traverse(scroll.base.supportedDirectoryStyles) { style =>
      segmentIds(scroll, style, new File(config.dataDir, s"raw/scroll${scroll.scrollId}/${scroll.base}-${style.productPrefix}-path-listing.html"))
        .map(_.map(segment => SegmentReference(scroll, segment)))
    }.map(_.flatten)

  val LinkR = """.*href="(.*)/".*""".r
  def segmentIds(scroll: ScrollReference, style: SegmentDirectoryStyle, targetFile: File): Future[Seq[String]] =
    cacheDownload(style.baseUrl(scroll), targetFile, ttl = 1.hour)
      .map { f =>
        scala.io.Source.fromFile(f).getLines().collect {
          case LinkR(dirName) if !dirName.startsWith("..") && style.isValidSegmentDirectory(dirName) =>
            style.segmentIdForDirectory(dirName)
        }.toVector
      }
      .recover {
        case _ => Vector.empty // FIXME: restrict to certain exceptions?
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
      case CrosscutWorkItemInput =>
        new File(dataDir, s"crosscuts/scroll${segment.scrollId}/${segment.segmentId}/crosscut.json")
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
  val TimesformerScroll5_27112024_17_32Input = InferenceWorkItemInput(TimesformerScroll5_27112024, Forward17Stride32)

  val Youssef_15_32Input = InferenceWorkItemInput(FirstWordModel, Forward15Stride32)
  val Youssef_15_32_ReverseInput = InferenceWorkItemInput(FirstWordModel, Reverse15Stride32)

  val Youssef_63_32Input = InferenceWorkItemInput(FirstWordModel, Forward63Stride32)
  val Youssef_63_32_ReverseInput = InferenceWorkItemInput(FirstWordModel, Forward63Stride32)

  def hasReasonableSize(info: SegmentInfo): Boolean =
    info.area.exists(_ > 8) || (info.width * info.height > 100 * 1000 * 1000) || info.scrollId == "172"

  type Filter = SegmentInfo => Boolean
  lazy val requestedWorkInputs: Seq[(WorkItemInput, Filter)] =
    Seq(
      //Youssef_15_32Input -> (s => s.scrollId == "1" && hasReasonableSize(s) && !s.ref.isHighResSegment /*|| s.scrollId == "2"*/ ),
      GrandPrize_17_32Input -> (s => (true /*!s.segmentId.startsWith("mesh")*/ /*|| s.scrollId == "172"*/ ) && hasReasonableSize(s) && !s.ref.isHighResSegment), //(s => s.scrollId == "1"),
      TimesformerScroll5_27112024_17_32Input -> (s => /*!s.segmentId.startsWith("mesh") &&*/ hasReasonableSize(s) && !s.ref.isHighResSegment), //(s => (s.scrollId == "172" && !s.segmentId.startsWith("mesh")) || hasReasonableSize(s)),
      //GrandPrizeFinetune0_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10)),
      //GrandPrizeFinetune1_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && hasReasonableSize(s) && !s.ref.isHighResSegment),
      //GrandPrizeFinetune2_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && s.area.exists(_ > 10)),
      //GrandPrizeFinetune3_17_32Input -> (s => Set("2", "0332", "1667")(s.scrollId) && hasReasonableSize(s) && !s.ref.isHighResSegment),
      //Youssef_15_32_ReverseInput -> (s => s.scrollId == "1" /*|| s.scrollId == "2"*/ ),
      //Youssef_63_32Input -> (s => s.scrollId == "332" || s.scrollId == "1667"),
      //Youssef_63_32_ReverseInput -> (s => s.scrollId == "332" || s.scrollId == "1667"),
      PPMFingerprintWorkItemInput -> (_.scrollId == "1"),
      DownSampleU16_2Input -> (_ => true),
      CrosscutWorkItemInput -> (_ => true),
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
