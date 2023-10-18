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

case class ImageInfo(scroll: Int, segmentId: String, width: Int, height: Int, area: Float) {
  def realScroll: Int = if (scroll == 3) 1 else scroll
}

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

  lazy val scroll1Segments: Future[Seq[ImageInfo]] =
    for {
      id1 <- segmentIds(1)
      id2 <- segmentIds(2)
      id3 <- segmentIds(3)
      infos <- Future.traverse(id1 ++ id2 ++ id3) { case (scroll, segment) => imageInfo(scroll, segment) }
    } yield infos.flatten

  lazy val mainRoute =
    concat(
      pathSingleSlash {
        get {
          onSuccess(scroll1Segments) { infos =>
            complete(html.home(infos))
          }
        }
      },
      path("license") {
        complete(html.license())
      },
      pathPrefix("scroll" / IntNumber / "segment" / Segment) { (scroll, segmentId) =>
        existingSegmentInfo(scroll, segmentId) { (info: ImageInfo) =>
          concat(
            pathSingleSlash {
              complete(html.segment(info))
            },
            path("mask") {
              onSuccess(resizedMask(scroll, segmentId)) { mask =>
                getFromFile(mask)
              }
            },
            pathPrefix(IntNumber) { z =>
              concat(
                path("dzi") {
                  val dziImage = DZIImage("jpg", 0, 512, info.width, info.height) // FIXME: imagesize
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

  def existing[T](value: Option[T]): Directive1[T] =
    value match {
      case Some(v) => provide(v)
      case None    => reject
    }

  def existingSegmentInfo(scroll: Int, segmentId: String): Directive1[ImageInfo] =
    onSuccess(imageInfo(scroll, segmentId)).flatMap(existing[ImageInfo])

  val InfoCache = LfuCache[(Int, String), Option[ImageInfo]]

  def imageInfo(scroll: Int, segmentId: String): Future[Option[ImageInfo]] =
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
  def _imageInfo(scroll: Int, segmentId: String): Future[Option[ImageInfo]] = {
    for {
      (width, height) <- sizeOf(scroll, segmentId)
      area <- areaFor(scroll, segmentId)
    } yield ImageInfo(scroll, segmentId, width, height, area)
  }.map(Some(_))
    .recover { case _: Throwable => None }

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
            /*if (segmentId == "20230827161847" || segmentId == "20231007101615")
              f"http://dl.ash2txt.org/hari-seldon-uploads/team-finished-paths/scroll$scroll/$segmentId/layers/$layer%02d.tif"
            else*/
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

  def resizedMask(scroll: Int, segmentId: String): Future[File] = {
    val target = new File(dataDir, s"raw/scroll$scroll/$segmentId/mask_small.png")
    cached(target, negTtlSeconds = 0) { () =>
      maskFor(scroll, segmentId).map { f =>
        import sys.process._
        val tmpFile = File.createTempFile("small_mask", ".png", target.getParentFile)
        val cmd = s"""vips thumbnail $f $tmpFile 10000 --height 50"""
        cmd.!!
        require(tmpFile.renameTo(target))
        target
      }
    }
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

  def maskFor(scroll: Int, segmentId: String): Future[File] =
    cacheDownload(
      f"${baseUrlFor(scroll)}$segmentId/${segmentId}_mask.png",
      new File(dataDir, s"raw/scroll$scroll/$segmentId/mask.png"))

  def areaFor(scroll: Int, segmentId: String): Future[Float] =
    cacheDownload(
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
    cacheDownload(baseUrl, targetFile, ttlSeconds = 3600).map { f =>
      scala.io.Source.fromFile(f).getLines().collect {
        case LinkR(segmentId) if !segmentId.startsWith("..") => segmentId
      }.toVector
    }
}
