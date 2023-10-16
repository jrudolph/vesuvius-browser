package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.http.scaladsl.model.{ HttpMethods, HttpRequest, StatusCodes, Uri, headers }
import org.apache.pekko.http.scaladsl.server.{ Directive1, Directives, PathMatchers, Route }
import org.apache.pekko.stream.scaladsl.{ FileIO, Source }
import org.apache.pekko.util.ByteString
import spray.json.*

import java.io.File
import scala.concurrent.Future
import scala.util.Success

case class ImageInfo(scroll: Int, segmentId: String, width: Int, height: Int, area: Float)

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

  val NameR = """(\d+)_(\d+).webp""".r

  lazy val scroll1Segments: Future[Seq[ImageInfo]] =
    for {
      id1 <- segmentIds(1)
      id2 <- segmentIds(2)
      id3 <- segmentIds(3)
      infos <- Future.traverse(id1 ++ id2 ++ id3) { case (scroll, segment) => imageInfo(scroll, segment).transform(Success(_)) }
    } yield infos.collect { case Success(s) => s }

  lazy val mainRoute =
    concat(
      pathSingleSlash {
        get {
          onSuccess(scroll1Segments) { infos =>
            complete(html.home(infos))
          }
        }
      },
      pathPrefix("scroll" / IntNumber / "segment" / Segment) { (scroll, segmentId) =>
        onSuccess(imageInfo(scroll, segmentId)) { info =>
          concat(
            pathSingleSlash {
              complete(html.segment(info))
            },
            path("mask") {
              onSuccess(maskFor(scroll, segmentId)) { mask =>
                getFromFile(mask)
              }
            },
            pathPrefix(IntNumber) { z =>
              concat(
                path("dzi") {
                  val dziImage = DZIImage("webp", 0, 512, info.width, info.height) // FIXME: imagesize
                  complete(JsObject("Image" -> dziImage.toJson))
                },
                path("dzi_files" / IntNumber / Segment) { (layer, name) =>
                  onSuccess(segmentLayer(scroll, segmentId, z)) { layerFile =>
                    val NameR(xStr, yStr) = name
                    val x = xStr.toInt
                    val y = yStr.toInt

                    onSuccess(resizer(layerFile, info, layer, x, y, z)) { resized =>
                      getFromFile(resized)
                    }
                  }
                }
              )
            }
          )
        }
      },
      getFromResourceDirectory("web")
    )

  val InfoCache = LfuCache[(Int, String), ImageInfo]

  def imageInfo(scroll: Int, segmentId: String): Future[ImageInfo] =
    InfoCache.getOrLoad((scroll, segmentId), _ => _imageInfo(scroll, segmentId))

  //areaFor
  def sizeOf(scroll: Int, segmentId: String): Future[(Int, Int)] =
    maskFor(scroll, segmentId)
      .map { f =>
        import sys.process._
        val cmd = s"vipsheader -a $f"
        val output = cmd.!!
        val kvs = output.split('\n').map(_.split(": ").map(_.trim)).map(a => a(0) -> a(1)).toMap
        val width = kvs("width").toInt
        val height = kvs("height").toInt
        (width, height)
      }(cpuBound)
  def _imageInfo(scroll: Int, segmentId: String): Future[ImageInfo] =
    for {
      (width, height) <- sizeOf(scroll, segmentId)
      area <- areaFor(scroll, segmentId)
      realScroll = if (scroll == 3) 1 else scroll
    } yield ImageInfo(realScroll, segmentId, width, height, area)

  val SegmentLayerCache = LfuCache[(Int, String, Int), File]
  def segmentLayer(scroll: Int, segmentId: String, layer: Int): Future[File] =
    SegmentLayerCache.getOrLoad((scroll, segmentId, layer), _ => _segmentLayer(scroll, segmentId, layer))

  def _segmentLayer(scroll: Int, segmentId: String, layer: Int): Future[File] = {
    val targetFile = new File(dataDir, s"raw/scroll$scroll/$segmentId/layers/$layer.jp2")
    targetFile.getParentFile.mkdirs()
    if (targetFile.exists) Future.successful(targetFile)
    else if (scroll == 1 && segmentId == "20230905134255" && layer == 31)
      Future.successful(new File("/home/johannes/git/opensource/_2023/Vesuvius-First-Letters/20230905134255_2_15.png"))
    else if (scroll == 1 && segmentId == "20230702185752" && layer == 31)
      Future.successful(new File("/home/johannes/git/opensource/_2023/Vesuvius-First-Letters/out.png"))
    //Future.successful(new File("/home/johannes/git/opensource/_2023/Vesuvius-First-Letters/20230702185752_2_15.png"))
    else {
      val webpVersion = new File(dataDir, s"raw/scroll$scroll/$segmentId/layers/$layer.webp")

      val tmpFile: Future[File] =
        if (webpVersion.exists())
          Future.successful(webpVersion)
        else {
          val url =
            if (segmentId == "20230827161847" || segmentId == "20231007101615")
              f"http://dl.ash2txt.org/hari-seldon-uploads/team-finished-paths/scroll$scroll/$segmentId/layers/$layer%02d.tif"
            else
              f"${baseUrlFor(scroll)}/$segmentId/layers/$layer%02d.tif"
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

  val ResizeCache = LfuCache[(File, ImageInfo, Int, Int, Int, Int), File]

  def resizer: ((File, ImageInfo, Int, Int, Int, Int)) => Future[File] =
    a => ResizeCache.getOrLoad(a, _ => ResizerQueue(a))

  val ResizerQueue = LifoQueue.semaphore[(File, ImageInfo, Int, Int, Int, Int), File](16) { (imageFile, info, layer, tileX, tileY, z) =>
    Future { resize(imageFile, info, layer, tileX, tileY, z) }(cpuBound)
  }

  def resize(imageFile: File, info: ImageInfo, layer: Int, tileX: Int, tileY: Int, z: Int): File = {
    val tileSize = 512
    val maxLayer = (math.log(info.width max info.height) / math.log(2)).ceil.toInt
    val size = 1 << (maxLayer - layer)
    val targetX = tileX * size * tileSize
    val targetY = tileY * size * tileSize

    val targetFile = new File(dataDir, s"tiles/scroll${info.scroll}/${info.segmentId}/layers/$z/$layer/${tileX}_$tileY.webp")
    if (targetFile.exists()) targetFile
    else {
      targetFile.getParentFile.mkdirs()
      val width = (info.width / size).min(tileSize).min((info.width - targetX) / size)
      val height = (info.height / size).min(tileSize).min((info.height - targetY) / size)

      import sys.process._
      val tmpFile1 = new File(dataDir, s"tiles/scroll${info.scroll}/${info.segmentId}/layers/$z/$layer/.tmp-${tileX}_$tileY.jp2")
      val tmpFile = new File(dataDir, s"tiles/scroll${info.scroll}/${info.segmentId}/layers/$z/$layer/.tmp-${tileX}_$tileY.webp")

      val cmd1 = s"""vips crop $imageFile "$tmpFile1[lossless]" $targetX $targetY ${size * width} ${size * height}"""
      println(f"Command for layers/$z/$layer/$tileX/$tileY: $cmd1")
      cmd1.!!

      val cmd2 = s"""vips thumbnail $tmpFile1 "$tmpFile[lossless]" $width"""
      println(f"Command2 for layers/$z/$layer/$tileX/$tileY: $cmd2")
      cmd2.!!

      tmpFile1.delete()
      tmpFile.renameTo(targetFile)
      targetFile
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

  def cacheFile(url: String, to: File, ttlSeconds: Long = 7200): Future[File] = {
    to.getParentFile.mkdirs()
    val neg = new File(to.getParentFile, s".neg-${to.getName}")
    if (to.exists() && to.lastModified() + ttlSeconds < System.currentTimeMillis()) Future.successful(to)
    else if (neg.exists() && neg.lastModified() + ttlSeconds < System.currentTimeMillis()) Future.failed(new RuntimeException(s"Negative cache for $url"))
    else
      download(url, to).recoverWith {
        case t: Throwable =>
          neg.getParentFile.mkdirs()
          neg.createNewFile()
          Future.failed(t)
      }
  }

  def maskFor(scroll: Int, segmentId: String): Future[File] =
    cacheFile(
      f"${baseUrlFor(scroll)}$segmentId/${segmentId}_mask.png",
      new File(dataDir, s"raw/scroll$scroll/$segmentId/mask.png"))

  def areaFor(scroll: Int, segmentId: String): Future[Float] =
    cacheFile(
      f"${baseUrlFor(scroll)}$segmentId/area_cm2.txt",
      new File(dataDir, s"raw/scroll$scroll/$segmentId/area_cm2.txt")
    ).map(f => scala.io.Source.fromFile(f).getLines().next().toFloat)

  def segmentIds(scroll: Int): Future[Seq[(Int, String)]] =
    segmentIds(baseUrlFor(scroll), new File(config.dataDir, s"raw/scroll$scroll/path-listing.html"))
      .map(_.map(id => scroll -> id))

  def baseUrlFor(scroll: Int): String =
    scroll match {
      case 1 | 2 => s"http://dl.ash2txt.org/full-scrolls/Scroll$scroll.volpkg/paths/"
      case 3     => "http://dl.ash2txt.org/hari-seldon-uploads/team-finished-paths/scroll1/"
    }

  val LinkR = """.*href="(.*)/".*""".r
  def segmentIds(baseUrl: String, targetFile: File): Future[Seq[String]] =
    cacheFile(baseUrl, targetFile).map { f =>
      scala.io.Source.fromFile(f).getLines().collect {
        case LinkR(segmentId) if !segmentId.startsWith("..") => segmentId
      }.toVector
    }
}
