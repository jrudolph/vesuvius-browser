package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.http.scaladsl.model.{ HttpMethods, HttpRequest, StatusCodes, Uri, headers }
import org.apache.pekko.http.scaladsl.server.{ Directive1, Directives, Route }
import org.apache.pekko.stream.scaladsl.{ FileIO, Flow, PartitionHub, Sink, Source }
import spray.json.*

import java.io.File
import scala.collection.immutable.ListMap
import scala.concurrent.Future
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

  lazy val main = encodeResponse(mainRoute)

  val NameR = """(\d+)_(\d+).jpg""".r

  lazy val scrollSegments: Future[Seq[ImageInfo]] =
    for {
      id1 <- segmentIds(FullScrollsBase, 1)
      id2 <- segmentIds(FullScrollsBase, 2)
      //id3 <- segmentIds(HariSeldonUploadsBase, 1)
      infos <- Future.traverse(id1 ++ id2)(imageInfo)
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
            complete(html.home(infos))
          }
        }
      },
      path("license") {
        complete(html.license())
      },
      pathPrefix("scroll" / IntNumber / "segment" / Segment) { (scroll, segmentId) =>
        resolveRef(scroll, segmentId) { segment =>

          imageInfo(segment).await.orReject { (info: ImageInfo) =>
            concat(
              pathSingleSlash {
                complete(html.segment(info))
              },
              path("mask") {
                resizedMask(segment).deliver
              },
              pathPrefix(IntNumber) { z =>
                concat(
                  path("dzi") {
                    val dziImage = DZIImage("jpg", 0, 512, info.width, info.height) // FIXME: imagesize
                    complete(JsObject("Image" -> dziImage.toJson))
                  },
                  path("dzi_files" / IntNumber / Segment) { (layer, name) =>
                    segmentLayer(segment, z).await { layerFile =>
                      val NameR(xStr, yStr) = name
                      val x = xStr.toInt
                      val y = yStr.toInt

                      resizer(layerFile, info, layer, x, y, z).deliver
                    }
                  }
                )
              }
            )
          }
        }
      },
      (post & pathPrefix("work")) {
        concat(
          path("next") {
            parameter("workerId") { workerId =>
              workItemManager.await { man =>
                val item = man.assignNext(workerId)
                println(s"Assigned $item to $workerId")
                complete(item)
              }
            }
          },
          path("result") {
            parameter("workerId", "workId".as[Int]) { (workerId, workItemId) =>
              workItemManager.await { man =>
                provide(man.findItem(workItemId)).orReject {
                  case item: InferenceWorkItem => // FIXME
                    println(s"Got result data request for $workItemId from $workerId, item: $item")

                    val file = new File(dataDir, s"inferred/scroll${item.segment.scroll}/${item.segment.segmentId}/inference_${item.model}_${item.startLayer}_${item.stride}.png")
                    file.getParentFile.mkdirs()
                    val tmpFile = File.createTempFile(".tmp.result", ".png", file.getParentFile)

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
      getFromResourceDirectory("web")
    )

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
    else {
      val webpVersion = new File(dataDir, s"raw/scroll$scroll/$segmentId/layers/$layer.webp")

      val tmpFile: Future[File] =
        if (webpVersion.exists())
          Future.successful(webpVersion)
        else {
          val url = f"${segment.baseUrl}layers/$layer%02d.tif"
          val tmpFile = File.createTempFile("download", ".tif", targetFile.getParentFile)
          download(url, tmpFile)
        }

      tmpFile
        .map { tmpFile =>
          val tmpFile2 = File.createTempFile("convert", ".jp2", targetFile.getParentFile)
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

  def resizer(imageFile: File, info: ImageInfo, layer: Int, tileX: Int, tileY: Int, z: Int): Future[File] =
    dzdir(imageFile, info, z).map { dir =>
      new File(dir, s"$layer/${tileX}_$tileY.jpg")
    }

  val DZDirCache = LfuCache[(File, ImageInfo, Int), File]
  def dzdir(imageFile: File, info: ImageInfo, z: Int): Future[File] =
    DZDirCache.getOrLoad((imageFile, info, z), _ => extractDZ(imageFile, info, z))

  def extractDZ(imageFile: File, info: ImageInfo, z: Int): Future[File] = {
    val targetDir = new File(dataDir, s"tiles-dz/scroll${info.scroll}/${info.segmentId}/layers/${z}_files")
    if (targetDir.exists()) Future.successful(targetDir)
    else Future {
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

  def resizedMask(segment: SegmentReference): Future[File] = {
    import segment._
    val target = new File(dataDir, s"raw/scroll$scroll/$segmentId/mask_small.png")
    cached(target, negTtlSeconds = 0) { () =>
      maskFor(segment).map { f =>
        import sys.process._
        val tmpFile = File.createTempFile("small_mask", ".png", target.getParentFile)
        val cmd = s"""vips thumbnail $f $tmpFile 10000 --height 50"""
        cmd.!!
        require(tmpFile.renameTo(target))
        target
      }
    }
  }

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

  def cached(to: File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl)(f: () => Future[File]): Future[File] = {
    to.getParentFile.mkdirs()
    val neg = new File(to.getParentFile, s".neg-${to.getName}")
    if (to.exists() && to.lastModified() + ttlSeconds * 1000 > System.currentTimeMillis()) Future.successful(to)
    else if (neg.exists() && neg.lastModified() + negTtlSeconds * 1000 > System.currentTimeMillis()) Future.failed(new RuntimeException(s"Negatively cached"))
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

  def inferenceInfoExistsFor(ref: SegmentReference, info: InferenceInfo): Boolean = {
    import ref._
    val target = new File(dataDir, s"inferred/scroll$scroll/$segmentId/inference_${info.model}_${info.startLayer}_${info.stride}.png")
    target.exists
  }

  case class InferenceInfo(model: String, startLayer: Int, stride: Int)
  lazy val requestedInferences: Seq[InferenceInfo] = Seq(InferenceInfo("youssef-test", 15, 32))

  lazy val workItems: Future[Seq[WorkItem]] =
    Source.futureSource(scrollSegments.map(x => Source(x.sortBy(_.segmentId).reverse)))
      .mapConcat(img => requestedInferences.map(info => img.ref -> info))
      .filter { case (ref, info) => !inferenceInfoExistsFor(ref, info) }
      .zipWithIndex
      .map { case ((ref, info), id) => InferenceWorkItem(id.toInt, ref, info.model, info.startLayer, info.stride) }
      .runWith(Sink.seq)

  lazy val workItemManager: Future[WorkItemManager] = workItems.map(WorkItemManager(_))
}

trait WorkItemManager {
  def assignNext(workerId: String): Option[WorkItem]

  def findItem(id: Int): Option[WorkItem]
  def markDone(workerId: String, workItem: WorkItem): Unit
}

object WorkItemManager {
  def apply(initialItems: Seq[WorkItem]): WorkItemManager = {
    sealed trait ItemState
    case object Queued extends ItemState
    case class Assigned(workerId: String, atMillis: Long) extends ItemState

    var itemState = ListMap[WorkItem, ItemState](initialItems.map(_ -> Queued): _*)

    new WorkItemManager {
      def assignNext(workerId: String): Option[WorkItem] =
        synchronized {
          itemState.find(_._2 == Queued).map {
            case (item, _) =>
              itemState = itemState.updated(item, Assigned(workerId, System.currentTimeMillis()))
              item
          }
        }

      def findItem(id: Int): Option[WorkItem] = itemState.keys.find(_.id == id)

      def markDone(workerId: String, workItem: WorkItem): Unit =
        synchronized {
          itemState = itemState.removed(workItem)
        }
    }
  }
}