package net.virtualvoid.vesuvius
package tiles

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.model.headers.ByteRange
import org.apache.pekko.http.scaladsl.model.*
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
import org.apache.pekko.stream.scaladsl.{ Sink, Source }
import org.apache.pekko.util.ByteString
import spray.json.*

import java.io.{ BufferedOutputStream, File, FileOutputStream }
import scala.concurrent.{ Future, Promise }

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
  import DefaultJsonProtocol.*
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
              parameter("x".as[Int], "y".as[Int], "z".as[Int], "bitmask".as[Int], "downsampling".as[Int]) { (x, y, z, bitmask, downsampling) =>
                block128x16(scroll, meta, x, y, z, bitmask, downsampling).deliver
              }
            }
          )
        }
      }
    }

  lazy val Scroll = IntNumber.flatMap(ScrollReference.byId)

  val MetadataCache = LfuCache[(ScrollReference, String), VolumeMetadata]
  def metadataForVolume(scroll: ScrollReference, volumeId: String): Future[VolumeMetadata] =
    MetadataCache.getOrLoad((scroll, volumeId), { _ =>
      downloadUtils.cacheDownload(
        scroll.volumeMetadataUrl(volumeId),
        new File(config.dataDir, s"metadata/${scroll.scroll}-${volumeId}.json")
      ).map { metadata =>
          scala.io.Source.fromFile(metadata).mkString.parseJson.convertTo[VolumeMetadata]
        }
    })

  val BlockCache = LfuCache[(ScrollReference, String, Int, Int, Int, Int, Int), File]
  def block128x16(scroll: ScrollReference, metadata: VolumeMetadata, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] =
    BlockCache.getOrLoad((scroll, metadata.uuid, x, y, z, bitmask, downsampling), _ => {
      val target = new File(config.dataDir, f"blocks/scroll${scroll.scroll}/${metadata.uuid}/128-16/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin")
      downloadUtils.cached(
        target
      )(() => createBlock128x16FromGrid(scroll, metadata, target, x, y, z, bitmask, downsampling))
    })

  val queue =
    Source.queue[(HttpRequest, Promise[HttpResponse])](20000)
      .mapAsyncUnordered(2000) {
        case (request, promise) =>
          Http().singleRequest(request).onComplete(promise.complete)
          promise.future
      }
      .to(Sink.ignore)
      .run()

  def queueRequest(request: HttpRequest): Future[HttpResponse] = {
    val promise = Promise[HttpResponse]()
    queue.offer(request -> promise)
    promise.future
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

        queueRequest(request).flatMap { response =>
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

    Future.traverse(0 until 128)(i =>
      downloadLayerSlice(z * 128 + i)
        .map { data =>
          println(s"Got slice $i from $x $y $z")
          data
        }
    ).map { slices =>
      println(s"Writing block $x $y $z")
      val res =
        writeBlock(target, 1) { (x, y, z) =>
          slices(z)(y * 128 + x)
        }
      println(s"Writing block $x $y $z done")
      res
    }
  }

  def createBlock128x16FromGrid(scroll: ScrollReference, meta: VolumeMetadata, target: File, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] = {
    val gridZSpacing = meta.uuid match {
      case "20231027191953" => 500262
      case "20230205180739" => 500147
    }

    def grid(i: Int): Int = (i * downsampling * 128) / 500 + 1
    def gridEnd(i: Int): Int = (((i * downsampling + downsampling) * 128) - 1) / 500 + 1

    val gridFilesNeeded: Seq[(Int, Int, Int)] =
      for {
        gx <- grid(x) to gridEnd(x)
        gy <- grid(y) to gridEnd(y)
        gz <- grid(z) to gridEnd(z)
      } yield (gx, gy, gz)

    val files =
      Future.traverse(gridFilesNeeded) {
        case id @ (gx, gy, gz) =>
          tileFor(scroll, meta, gx, gy, gz)
            .map { target =>
              val raf = new java.io.RandomAccessFile(target, "r")
              val mmap = raf.getChannel.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, 8, raf.length() - 8)
              id -> mmap
            }
      }.map(_.toMap)

    files.map { gridMap =>
      println(s"Writing block $x $y $z")
      val res =
        writeBlock(target, downsampling) { (lx, ly, lz) =>
          val globalX = (x * 128 + lx) * downsampling
          val globalY = (y * 128 + ly) * downsampling
          val globalZ = (z * 128 + lz) * downsampling

          val gx = globalX / 500 + 1
          val gy = globalY / 500 + 1
          val gz = globalZ / 500 + 1

          val data = gridMap((gx, gy, gz))
          val tx = globalX % 500
          val ty = globalY % 500
          val tz = globalZ % 500

          val offset = gridZSpacing * tz + (ty * 500 + tx) * 2

          /*val u16 = data.get(offset) & 0xff |
            (data.get(offset + 1) & 0xff) << 8

          ((u16 - 0x5000).max(0) / 0x90).min(255)*/
          data.get(offset + 1) & bitmask
        }
      println(s"Writing block $x $y $z done")
      res
    }
  }

  lazy val TileCache = LfuCache[(ScrollReference, String, Int, Int, Int), File]
  def tileFor(scroll: ScrollReference, meta: VolumeMetadata, gx: Int, gy: Int, gz: Int): Future[File] =
    TileCache.getOrLoad((scroll, meta.uuid, gx, gy, gz), _ => {
      val url = f"${scroll.volumeGridUrl(meta.uuid)}cell_yxz_$gy%03d_$gx%03d_$gz%03d.tif"
      val target = new File(config.dataDir, f"grid/scroll${scroll.scroll}/${meta.uuid}/cell_yxz_$gy%03d_$gx%03d+$gz%03d.tif")
      downloadUtils.cacheDownload(
        url,
        target,
        ttlSeconds = 5L * 365 * 24 * 3600
      )
    })

  def writeBlock(target: File, downsampling: Int)(data: (Int, Int, Int) => Int): File = {
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

      val value = data(x, y, z)
      fos.write(value)

      i += 1
    }

    fos.close()
    tmp.renameTo(target)
    target
  }
}
