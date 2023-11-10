package net.virtualvoid.vesuvius
package tiles

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.model.headers.ByteRange
import org.apache.pekko.http.scaladsl.model.{ HttpMethods, HttpRequest, Multipart, headers }
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
import org.apache.pekko.util.ByteString
import spray.json.*

import java.awt.image.BufferedImage
import java.io.{ BufferedOutputStream, File, FileOutputStream }
import scala.concurrent.Future
import scala.util.Success

case class VolumeMetadata(
    name:      String,
    uuid:      String,
    width:     Int,
    height:    Int,
    slices:    Int,
    `type`:    String,
    min:       Double,
    max:       Double,
    voxelsize: Double)
object VolumeMetadata {
  import DefaultJsonProtocol._
  implicit val format: RootJsonFormat[VolumeMetadata] = jsonFormat9(apply)
}

class TilesRoutes(config: TilesConfig)(implicit system: ActorSystem) extends SprayJsonSupport {
  import system.dispatcher
  val downloadUtils = new DownloadUtils(config)

  lazy val main: Route =
    tilesRoutes

  lazy val tilesRoutes =
    pathPrefix("tiles") {
      pathPrefix("scroll" / Scroll / "volume" / Segment) { (scroll, volume) =>
        metadataForVolume(scroll, volume).await { meta =>
          concat(
            pathSingleSlash {
              complete(meta)
            },
            path("download" / "128-16") {
              parameter("x".as[Int], "y".as[Int], "z".as[Int]) { (x, y, z) =>
                block128x16(scroll, meta, x, y, z).deliver
                /*
                complete("Hello")*/
              }
            }
          )
        }
      }
    }

  lazy val Scroll = IntNumber.flatMap(ScrollReference.byId)

  def metadataForVolume(scroll: ScrollReference, volumeId: String): Future[VolumeMetadata] =
    downloadUtils.cacheDownload(
      scroll.volumeMetadataUrl(volumeId),
      new File(config.dataDir, s"metadata/${scroll.scroll}-${volumeId}.json")
    ).map { metadata =>
        scala.io.Source.fromFile(metadata).mkString.parseJson.convertTo[VolumeMetadata]
      }

  def block128x16(scroll: ScrollReference, metadata: VolumeMetadata, x: Int, y: Int, z: Int): Future[File] = {
    val target = new File(config.dataDir, f"blocks/scroll${scroll.scroll}/${metadata.uuid}/128-16/z$z%03d/xyz-$x%03d-$y%03d-$z%03d.bin")
    downloadUtils.cached(
      target
    )(() => createBlock128x16(scroll, metadata, target, x, y, z))
  }

  def createBlock128x16(scroll: ScrollReference, meta: VolumeMetadata, target: File, x: Int, y: Int, z: Int): Future[File] = {
    def downloadLayerSlice(z: Int): Future[Array[Byte]] = {
      val rangeStarts =
        for {
          i <- 0 until 128
        } yield 8 + (meta.width * (y * 128 + i) + (x * 128)) * 2 /* u16 */

      val ranges = rangeStarts.map { starts =>
        headers.ByteRange(starts, starts + (128 * 2) /*u16*/ - 1 /* inclusive */ )
      }

      def requestRanges(ranges: Seq[ByteRange]): Future[ByteString] = {
        val rangeHeader = headers.`Range`(ranges)
        val request =
          HttpRequest(HttpMethods.GET, uri = f"${scroll.volumeUrl(meta.uuid)}$z%05d.tif", headers = Seq(rangeHeader, downloadUtils.auth))

        Http().singleRequest(request).flatMap { response =>
          Unmarshal(response).to[Multipart.ByteRanges].flatMap { ranges =>
            ranges.parts
              .flatMapConcat(_.entity.dataBytes)
              .runFold(ByteString.empty)(_ ++ _)

          }
        }
      }

      Future.traverse(ranges.grouped(128 / config.requestsPerLayer))(requestRanges)
        //.flatMap(x => requestRanges(x.toSeq))
        .map(_.foldLeft(ByteString.empty)(_ ++ _).toArray)
        .map { u16a =>
          Array.tabulate[Byte](128 * 128)(i => (u16a(i * 2 + 1) & 0xff).toByte)
        }
    }

    def writeBlock(slices: Seq[Array[Byte]]): File = {
      println(s"Writing block $x $y $z")
      target.getParentFile.mkdirs()
      val tmp = File.createTempFile(".tmp.block", ".bin", target.getParentFile)
      val fos = new BufferedOutputStream(new FileOutputStream(tmp))

      var i = 0
      while (i < 128 * 128 * 128) {
        // 2^21 = 2097152, 2^12 = 4096, numBlocks = 512 = 2^9, 8 blocks in each direction
        val bz = i >> 18
        val by = (i >> 15) & 0x7
        val bx = (i >> 12) & 0x7

        val pz = (i >> 8) & 0xf
        val py = (i >> 4) & 0xf
        val px = i & 0xf

        val x = bx * 16 + px
        val y = by * 16 + py
        val z = bz * 16 + pz

        fos.write(slices(z)(y * 128 + x))
        i += 1
      }

      fos.close()
      println(s"Writing block $x $y $z done")
      tmp.renameTo(target)
      target
    }

    Future.traverse(0 until 128)(i =>
      downloadLayerSlice(z * 128 + i)
        .map { data =>
          println(s"Got slice $i from $x $y $z")
          data
        }
    ).map(writeBlock)
    /*.onComplete {
        case Success(bufs) =>
          val buf = bufs(0)
          println(buf.size)
          val image = new BufferedImage(256, 256, BufferedImage.TYPE_INT_ARGB)
          for {
            y <- 0 until 256
            x <- 0 until 256
          } {
            val value = buf(y * 256 + x) & 0xff
            image.setRGB(x, y, 0xff000000 | (value << 16) | (value << 8) | (value << 0))
          }
          javax.imageio.ImageIO.write(image, "png", new File(s"test-image.png"))
      }

    ???*/
  }
}
