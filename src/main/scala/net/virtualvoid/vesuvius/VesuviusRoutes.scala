package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.http.scaladsl.model.{HttpMethods, HttpRequest, StatusCodes, Uri, headers}
import org.apache.pekko.http.scaladsl.server.{Directive1, Directives, PathMatchers, Route}
import org.apache.pekko.stream.scaladsl.{FileIO, Source}
import org.apache.pekko.util.ByteString
import spray.json.*

import java.io.File
import scala.concurrent.Future

case class ImageInfo(scroll: Int, segmentId: String, width: Int, height: Int)

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

class VesuviusRoutes()(implicit system: ActorSystem) extends Directives with TwirlSupport with SprayJsonSupport {
  import system.dispatcher

  val dataDir = new File("data")

  lazy val main = encodeResponse(mainRoute)

  val NameR = """(\d+)_(\d+).webp""".r

  lazy val mainRoute =
    concat(
      pathSingleSlash {
        get {
          complete {
            html.home()
          }
        }
      },
      pathPrefix("scroll" / IntNumber / "segment" / Segment) { (scroll, segmentId) =>
        onSuccess(imageInfo(scroll, segmentId)) { info =>
          concat(
            pathSingleSlash {
              complete(html.segment(info))
            },
            pathPrefix(IntNumber) { z =>
              onSuccess(segmentLayer(scroll, segmentId, z)) { layerFile =>
                concat(
                  path("dzi") {
                    val dziImage = DZIImage("webp", 0, 512, info.width, info.height) // FIXME: imagesize
                    complete(JsObject("Image" -> dziImage.toJson))
                  },
                  path("dzi_files" / IntNumber / Segment) { (layer, name) =>
                    val NameR(xStr, yStr) = name
                    val x = xStr.toInt
                    val y = yStr.toInt

                    val resized = resize(layerFile, info, layer, x, y, z)

                    getFromFile(resized)
                  },
                )
              }
            }
          )
        }
      },
      getFromResourceDirectory("web")
    )

  val InfoCache = LfuCache[(Int, String), ImageInfo]

  def imageInfo(scroll: Int, segmentId: String): Future[ImageInfo] =
    InfoCache.getOrLoad((scroll, segmentId), _ => _imageInfo(scroll, segmentId))

  def _imageInfo(scroll: Int, segmentId: String): Future[ImageInfo] =
    segmentLayer(scroll, segmentId, 32)
      .map { f =>
        import sys.process._
        val cmd = s"identify -format '%w %h' $f"
        val output = cmd.!!
        val Array(width, height) = output.trim.split(' ')
        ImageInfo(scroll, segmentId, width.toInt, height.toInt)
      }

  val SegmentLayerCache = LfuCache[(Int, String, Int), File]
  def segmentLayer(scroll: Int, segmentId: String, layer: Int): Future[File] =
    SegmentLayerCache.getOrLoad((scroll, segmentId, layer), _ => _segmentLayer(scroll, segmentId, layer))

  def _segmentLayer(scroll: Int, segmentId: String, layer: Int): Future[File] = {
    val targetFile = new File(dataDir, s"raw/scroll$scroll/$segmentId/layers/$layer.webp")
    if (targetFile.exists) Future.successful(targetFile)
    else {
      val url = s"http://dl.ash2txt.org/full-scrolls/Scroll$scroll.volpkg/paths/$segmentId/layers/$layer.tif"
      val tmpFile = File.createTempFile("download", ".tif")

      download(url, tmpFile)
        .map { f =>
          val tmpFile2 = File.createTempFile("convert", ".webp")
          import sys.process._
          val cmd = s"""convert $f -define webp:lossless=true $tmpFile2"""
          println(s"Convert big image to webp: $cmd")
          cmd.!!
          targetFile.getParentFile.mkdirs()
          tmpFile2.renameTo(targetFile)
          tmpFile.delete()
          targetFile
        }
    }
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
      val width = (info.width / size).min(tileSize)
      val height = (info.height / size).min(tileSize)

      import sys.process._
      val tmpFile = new File(dataDir, s"tiles/scroll${info.scroll}/${info.segmentId}/layers/$z/$layer/.tmp-${tileX}_$tileY.webp")

      val cmd = s"""convert $imageFile -crop ${size * width}x${size * height}+$targetX+$targetY -resize ${width}x$height -define webp:lossless=true $tmpFile"""
      println(f"Command for layers/$z/$layer/$tileX/$tileY: $cmd")
      cmd.!!

      tmpFile.renameTo(targetFile)
      targetFile
    }
  }

  val auth = headers.Authorization(headers.BasicHttpCredentials("blip", "blup"))
  def download(url: String, to: File): Future[File] = {
    val tmpFile = new File(to.getParentFile, s".tmp-${to.getName}")
    Http().singleRequest(HttpRequest(HttpMethods.GET, Uri(url), headers = auth :: Nil))
      .flatMap { res =>
        require(res.status == StatusCodes.OK, s"Got status ${res.status} for $url")
        res.entity.dataBytes.runWith(FileIO.toPath(tmpFile.toPath))
      }
      .map { _ => tmpFile.renameTo(to); to }
  }
}
