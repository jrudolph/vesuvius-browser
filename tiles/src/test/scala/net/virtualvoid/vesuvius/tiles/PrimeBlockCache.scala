package net.virtualvoid.vesuvius.tiles

import net.virtualvoid.vesuvius.{ DownloadUtils, ScrollReference, VolumeMetadataRepository }
import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ HttpRequest, StatusCodes }
import org.apache.pekko.stream.scaladsl.Source

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future
import scala.concurrent.duration.*

object PrimeBlockCache extends App {
  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher
  val config = TilesConfig.fromConfig(system.settings.config)

  val downloadUtils = new DownloadUtils(config)
  val metadataRepo = new VolumeMetadataRepository(downloadUtils, config.dataDir)

  import metadataRepo.metadataForVolume

  def gridFilesNeeded(x: Int, y: Int, z: Int, downsampling: Int): Seq[(Int, Int, Int)] = {
    def grid(i: Int): Int = (i * downsampling * 64) / 500 + 1

    def gridEnd(i: Int): Int = (((i * downsampling + downsampling) * 64) - 1) / 500 + 1

    for {
      gx <- grid(x) to gridEnd(x)
      gy <- grid(y) to gridEnd(y)
      gz <- grid(z) to gridEnd(z)
    } yield (gx, gy, gz)
  }

  type C3 = (Int, Int, Int)

  def ensureTile(scroll: ScrollReference, volume: String, coord: C3, quant: Int): Future[Done] = {
    // /tiles/scroll/{}/volume/{}/download/64-4?x={}&y={}&z={}&bitmask={}&downsampling={}",
    val url = s"https://vesuvius.virtual-void.net/tiles/scroll/${scroll.scrollId}/volume/$volume/download/64-4?x=${coord._1}&y=${coord._2}&z=${coord._3}&bitmask=255&downsampling=$quant"
    val req = HttpRequest(uri = url, headers = downloadUtils.auth :: Nil)
    //println(f"At $scroll $coord $quant")
    Http().singleRequest(req).flatMap { res =>
      if (res.status.intValue == 200) {
        //println(s"Downloaded $scroll $coord $quant")
        res.discardEntityBytes()
        Future.successful(Done)
      } else if (res.status == StatusCodes.EnhanceYourCalm) {
        //println(s"Got 429 for $scroll $coord $quant, retrying in 10s")
        val promise = scala.concurrent.Promise[Done]()
        system.scheduler.scheduleOnce(10.seconds) {
          promise.completeWith(ensureTile(scroll, volume, coord, quant))
        }
        promise.future
      } else {
        println(s"Failed to download $url: ${res.status}")
        res.discardEntityBytes()
        Future.successful(Done)
      }
    }.recover {
      case t =>
        println(s"Failed to download $url: $t")
        Done
    }
  }

  def zorder0(x: Int, y: Int, z: Int): Int = {
    var x1 = x
    var y1 = y
    var z1 = z
    var answer = 0
    var i = 0
    while (i < 10) {
      answer |= (x1 & 1) << (3 * i)
      answer |= (y1 & 1) << (3 * i + 1)
      answer |= (z1 & 1) << (3 * i + 2)
      x1 >>= 1
      y1 >>= 1
      z1 >>= 1
      i += 1
    }
    answer
  }

  def primeVolume(scroll: ScrollReference, volume: String, quants: Seq[Int]): Future[Done] =
    metadataForVolume(scroll, volume).flatMap { meta =>
      println(s"Got meta for $volume: $meta")
      /*val centerGrid = (meta.width / 2 / 500, meta.height / 2 / 500, meta.slices / 2 / 500)
      def distanceToCenter(gx: Int, gy: Int, gz: Int): Long = {
        /*val dx = (gx - centerGrid._1).toLong
        val dy = (gy - centerGrid._2).toLong
        val dz = (gz - centerGrid._3).toLong
        dx * dx + dy * dy + dz * dz*/
        val
      }*/

      val tileGrids: Seq[((Int, Int, Int, Int), C3)] =
        for {
          quant <- quants
          tileSize = 64 * quant
          x <- 0 until meta.width / tileSize
          y <- 0 until meta.height / tileSize
          z <- 0 until meta.slices / tileSize
          (gx, gy, gz) <- gridFilesNeeded(x, y, z, quant)
        } yield (x, y, z, quant) -> (gx, gy, gz)

      println(s"Need to download ${tileGrids.size} tiles")
      val elems = tileGrids.groupBy(_._2).toVector.sortBy(x => (zorder0 _).tupled.apply(x._1)).flatMap(_._2.map(_._1).toVector.sortBy(x => (x._4, zorder0(x._1, x._2, x._3))))

      val n = new AtomicInteger(0)
      Source(elems.reverse)
        .mapAsyncUnordered(8) {
          case o @ (x, y, z, quant) =>
            ensureTile(scroll, volume, (x, y, z), quant).map(res => o -> res)
        }
        .runForeach {
          case o @ (x, y, z, quant) -> res =>
            val i = n.incrementAndGet()
            if (i % 25 == 0) {
              println(f"Downloaded $i%5d / ${elems.size} tiles (${i * 100.0 / elems.size}%5.2f%%) Last: $scroll $x $y $z $quant")
            }
        }
    }

  val s1 = ScrollReference.scrolls(0)
  primeVolume(s1, s1.defaultVolumeId, Seq(8, 16))
    .onComplete(println)
}
