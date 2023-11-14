package net.virtualvoid.vesuvius
package tiles

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import spray.json.*

import java.io.{ BufferedOutputStream, File, FileOutputStream }
import scala.concurrent.Future

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
            path("download" / "64-4") {
              parameter("x".as[Int], "y".as[Int], "z".as[Int], "bitmask".as[Int], "downsampling".as[Int]) { (x, y, z, bitmask, downsampling) =>
                block64x4(scroll, meta, x, y, z, bitmask, downsampling).deliver
              }
            }
          )
        }
      }
    }

  lazy val Scroll = IntNumber.flatMap(ScrollReference.byId)

  val MetadataCache = downloadUtils.downloadCache[(ScrollReference, String)](
    { case (scroll, volumeId) => scroll.volumeMetadataUrl(volumeId) },
    { case (scroll, volumeId) => new File(config.dataDir, s"metadata/${scroll.scroll}-${volumeId}.json") }
  )
  def metadataForVolume(scroll: ScrollReference, volumeId: String): Future[VolumeMetadata] =
    MetadataCache((scroll, volumeId))
      .map { metadata =>
        scala.io.Source.fromFile(metadata).mkString.parseJson.convertTo[VolumeMetadata]
      }

  val BlockCache = downloadUtils.computeCache[(ScrollReference, VolumeMetadata, Int, Int, Int, Int, Int)](
    { case (scroll, meta, x, y, z, bitmask, downsampling) => new File(config.dataDir, f"blocks/scroll${scroll.scroll}/${meta.uuid}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") },
    CacheSettings.Default.copy(negTtlSeconds = 60)
  ) {
      case (scroll, metadata, x, y, z, bitmask, downsampling) =>
        val target = new File(config.dataDir, f"blocks/scroll${scroll.scroll}/${metadata.uuid}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") // FIXME: dry
        createBlock64x4FromGrid(scroll, metadata, target, x, y, z, bitmask, downsampling)
    }
  def block64x4(scroll: ScrollReference, metadata: VolumeMetadata, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] =
    BlockCache((scroll, metadata, x, y, z, bitmask, downsampling))

  def createBlock64x4FromGrid(scroll: ScrollReference, meta: VolumeMetadata, target: File, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] = {
    val gridZSpacing = meta.uuid match {
      case "20230205180739" => 500147 // scroll 1
      case "20230210143520" => 500147 // scroll 2 fine
      case "20230212125146" => 500147 // scroll 2 coarse
      case "20231027191953" => 500262 // scroll 0332
      case "20231107190228" => 500262 // scroll 1667
    }

    def grid(i: Int): Int = (i * downsampling * 64) / 500 + 1
    def gridEnd(i: Int): Int = (((i * downsampling + downsampling) * 64) - 1) / 500 + 1

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
          val globalX = (x * 64 + lx) * downsampling
          val globalY = (y * 64 + ly) * downsampling
          val globalZ = (z * 64 + lz) * downsampling

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

  lazy val TileCache = downloadUtils.downloadCache[(ScrollReference, String, Int, Int, Int)](
    { case (scroll, uuid, gx, gy, gz) => f"${scroll.volumeGridUrl(uuid)}cell_yxz_$gy%03d_$gx%03d_$gz%03d.tif" },
    { case (scroll, uuid, gx, gy, gz) => new File(config.dataDir, f"grid/scroll${scroll.scroll}/$uuid/cell_yxz_$gy%03d_$gx%03d+$gz%03d.tif") },
    maxConcurrentRequests = config.maxConcurrentGridRequests,
    settings = CacheSettings.Default.copy(
      negTtlSeconds = 60,
      baseDirectory = Some(new File(config.dataDir, "grid")),
      maxCacheSize = config.gridCacheMaxSize,
      cacheHighWatermark = config.gridCacheHighWatermark,
      cacheLowWatermark = config.gridCacheLowWatermark
    )
  )
  def tileFor(scroll: ScrollReference, meta: VolumeMetadata, gx: Int, gy: Int, gz: Int): Future[File] =
    TileCache((scroll, meta.uuid, gx, gy, gz))

  def writeBlock(target: File, downsampling: Int)(data: (Int, Int, Int) => Int): File = {
    target.getParentFile.mkdirs()
    val tmp = File.createTempFile(".tmp.block", ".bin", target.getParentFile)
    val fos = new BufferedOutputStream(new FileOutputStream(tmp))

    var i = 0
    while (i < 64 * 64 * 64) {
      // 2^21 = 2097152, 2^12 = 4096, numBlocks = 512 = 2^9, 8 blocks in each direction
      // 128^3 in 16^3 blocks: zzzzzzzyyyyyyyxxxxxxx -> zzzyyyxxxzzzzyyyyxxxx
      //  64^3 in  4^3 blocks:    zzzzzzyyyyyyxxxxxx ->    zzzzyyyyxxxxzzyyxx
      val bz = i >> 14
      val by = (i >> 10) & 0xf
      val bx = (i >> 6) & 0xf

      val pz = (i >> 4) & 0x3
      val py = (i >> 2) & 0x3
      val px = i & 0x3

      val x = bx * 4 + px
      val y = by * 4 + py
      val z = bz * 4 + pz

      val value = data(x, y, z)
      fos.write(value)

      i += 1
    }

    fos.close()
    tmp.renameTo(target)
    target
  }
}
