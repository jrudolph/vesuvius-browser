package net.virtualvoid.vesuvius
package tiles

import net.virtualvoid.unix.Mmap
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import sun.nio.ch.DirectBuffer

import java.io.{ BufferedOutputStream, File, FileOutputStream }
import java.nio.MappedByteBuffer
import scala.concurrent.Future

class TilesRoutes(config: TilesConfig)(implicit system: ActorSystem) extends SprayJsonSupport {
  import system.dispatcher

  val blockingDispatcher = system.dispatchers.lookup("pekko.actor.default-blocking-io-dispatcher")
  val MadviseCoalescePageGap = system.settings.config.getInt("app.madvise-coalesce-page-gap")

  val downloadUtils = new DownloadUtils(config)
  val metadataRepo = new VolumeMetadataRepository(downloadUtils, config.dataDir)
  import metadataRepo.metadataForVolume

  lazy val main: Route =
    tilesRoutes

  lazy val tilesRoutes =
    pathPrefix("tiles") {
      concat(
        pathPrefix("scroll" / Scroll / "volume" / Segment) { (scroll, volume) =>
          metadataForVolume(scroll, volume).await { meta =>
            concat(
              pathSingleSlash {
                complete(meta)
              },
              path("download" / "64-4") {
                parameter("x".as[Int], "y".as[Int], "z".as[Int], "bitmask".as[Int], "downsampling".as[Int]) { (x, y, z, bitmask, downsampling) =>
                  if (scroll.base != FragmentsBase && scroll.base != FragmentsBase && volume != "20231117161658" && volume != "20230206171837") {
                    if (block64x4IsAvailable(scroll, meta, x, y, z, bitmask, downsampling) || gridFileAvailableFor(scroll, meta, x, y, z, downsampling))
                      block64x4(scroll, meta, x, y, z, bitmask, downsampling).deliver
                    else {
                      // refresh request for grid files for this block
                      gridTilesNeeded(scroll, meta, x, y, z, downsampling).foreach(GridTileCache(_))
                      //block64x4(scroll, meta, x, y, z, bitmask, downsampling)
                      complete(StatusCodes.EnhanceYourCalm)
                    }
                  } else {
                    if (volumeBlock64x4IsAvailable(scroll, meta, x, y, z, bitmask, downsampling) || volumeLayersAvailableFor(scroll, meta, z, downsampling))
                      volumeBlock64x4(scroll, meta, x, y, z, bitmask, downsampling).deliver
                    // optionally: generate on the fly instead of using cached variant
                    //complete(createVolumeBlock64x4FromLayersToBytes(scroll, meta, x, y, z, bitmask, downsampling))
                    else {
                      // refresh request for grid files for this block
                      volumeLayersNeeded(z, downsampling).foreach(VolumeLayerCache(scroll, meta, _))
                      //volumeBlock64x4(scroll, meta, x, y, z, bitmask, downsampling)
                      complete(StatusCodes.EnhanceYourCalm)
                    }
                  }
                }
              }
            )
          }
        },
        pathPrefix("scroll" / Scroll / "segment" / Segment) { (scroll, segment) =>
          path("download" / "64-4") {
            parameter("x".as[Int], "y".as[Int], "z".as[Int], "bitmask".as[Int], "downsampling".as[Int]) { (x, y, z, bitmask, downsampling) =>
              block64x4Surface(SegmentReference(scroll, segment), x, y, z, bitmask, downsampling).await.orReject.deliver
            }
          }
        }
      )
    }

  lazy val Scroll = Segment.flatMap(ScrollReference.byId)

  val BlockCache = downloadUtils.computeCache[(ScrollReference, VolumeMetadata, Int, Int, Int, Int, Int)](
    { case (scroll, meta, x, y, z, bitmask, downsampling) => new File(config.dataDir, f"blocks/scroll${scroll.scrollId}/${meta.uuid}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") },
    CacheSettings.Default.copy(negTtlSeconds = 60)
  ) {
      case (scroll, metadata, x, y, z, bitmask, downsampling) =>
        val target = new File(config.dataDir, f"blocks/scroll${scroll.scrollId}/${metadata.uuid}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") // FIXME: dry
        createBlock64x4FromGrid(scroll, metadata, target, x, y, z, bitmask, downsampling)
    }
  def block64x4(scroll: ScrollReference, metadata: VolumeMetadata, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] =
    BlockCache((scroll, metadata, x, y, z, bitmask, downsampling))

  def block64x4IsAvailable(scroll: ScrollReference, meta: VolumeMetadata, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Boolean =
    BlockCache.isReady((scroll, meta, x, y, z, bitmask, downsampling))

  def gridFileAvailableFor(scroll: ScrollReference, meta: VolumeMetadata, x: Int, y: Int, z: Int, downsampling: Int): Boolean =
    gridFilesNeeded(x, y, z, downsampling).map(gridFile(scroll, meta.uuid, _, _, _)).forall(_.exists())

  def gridFilesNeeded(x: Int, y: Int, z: Int, downsampling: Int): Seq[(Int, Int, Int)] = {
    def grid(i: Int): Int = (i * downsampling * 64) / 500 + 1
    def gridEnd(i: Int): Int = (((i * downsampling + downsampling) * 64) - 1) / 500 + 1

    for {
      gx <- grid(x) to gridEnd(x)
      gy <- grid(y) to gridEnd(y)
      gz <- grid(z) to gridEnd(z)
    } yield (gx, gy, gz)
  }
  def gridTilesNeeded(scroll: ScrollReference, meta: VolumeMetadata, x: Int, y: Int, z: Int, downsampling: Int): Seq[(ScrollReference, String, Int, Int, Int)] =
    gridFilesNeeded(x, y, z, downsampling).map { case (gx, gy, gz) => (scroll, meta.uuid, gx, gy, gz) }

  def createBlock64x4FromGrid(scroll: ScrollReference, meta: VolumeMetadata, target: File, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] = {
    val gridZSpacing = meta.uuid match {
      case "20230205180739" => 500147 // scroll 1
      case "20230210143520" => 500147 // scroll 2 fine
      case "20230212125146" => 500147 // scroll 2 coarse
      case "20231027191953" => 500262 // scroll 0332
      case "20231107190228" => 500262 // scroll 1667
    }

    val files =
      Future.traverse(gridFilesNeeded(x, y, z, downsampling)) {
        case id @ (gx, gy, gz) =>
          tileFor(scroll, meta, gx, gy, gz)
            .map { target =>
              val raf = new java.io.RandomAccessFile(target, "r")
              val mmap = raf.getChannel.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, 8, raf.length() - 8)
              madviseSequential(mmap)
              id -> mmap
            }
      }.map(_.toMap)

    files.flatMap { gridMap =>
      println(s"Writing block $x $y $z q$downsampling")
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
      println(s"Writing block $x $y $z q$downsampling done")
      res
    }
  }
  def gridFile(scroll: ScrollReference, uuid: String, gx: Int, gy: Int, gz: Int): File =
    new File(config.dataDir, f"grid/scroll${scroll.scrollId}/$uuid/cell_yxz_$gy%03d_$gx%03d_$gz%03d.tif")

  lazy val GridTileCache = downloadUtils.downloadCache[(ScrollReference, String, Int, Int, Int)](
    { case (scroll, uuid, gx, gy, gz) => f"${scroll.volumeGridUrl(uuid)}cell_yxz_$gy%03d_$gx%03d_$gz%03d.tif" },
    { case (scroll, uuid, gx, gy, gz) => gridFile(scroll, uuid, gx, gy, gz) },
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
    GridTileCache((scroll, meta.uuid, gx, gy, gz))

  def writeBlock(target: File, downsampling: Int)(data: (Int, Int, Int) => Int): Future[File] = Future {
    target.getParentFile.mkdirs()
    val tmp = File.createTempFile(".tmp.block", ".bin", target.getParentFile)
    val fos = new BufferedOutputStream(new FileOutputStream(tmp), 300000) // buffer full file before writing to avoid too many syncs

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
  }(blockingDispatcher)

  def blockBytes(downsampling: Int)(data: (Int, Int, Int) => Int): Future[Array[Byte]] = Future {
    val res = new Array[Byte](64 * 64 * 64)

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

      res(i) = data(x, y, z).toByte

      i += 1
    }

    res
  }(blockingDispatcher)

  val SurfaceBlockCache = downloadUtils.computeCache[(SegmentReference, Int, Int, Int, Int, Int)](
    { case (segment, x, y, z, bitmask, downsampling) => new File(config.dataDir, f"blocks/scroll${segment.scrollId}/segment/${segment.segmentId}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") },
    CacheSettings.Default.copy(negTtlSeconds = 60)
  ) {
      case (segment, x, y, z, bitmask, downsampling) =>
        val target = new File(config.dataDir, f"blocks/scroll${segment.scrollId}/segment/${segment.segmentId}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") // FIXME: DRY
        block64x4SurfaceFromLayers(segment, target, x, y, z, bitmask, downsampling)
    }

  def block64x4Surface(segment: SegmentReference, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[Option[File]] =
    SurfaceBlockCache((segment, x, y, z, bitmask, downsampling))
      .map(Some(_))
      .recover {
        case _: NoSuchElementException => None
      }

  def block64x4SurfaceFromLayers(segment: SegmentReference, target: File, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] = {
    layersForSegment(segment).flatMap { layers =>
      val maps = layers.map { l =>
        val raf = new java.io.RandomAccessFile(l, "r")
        val map = raf.getChannel.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, 8, raf.length() - 8)
        madviseSequential(map)
        map
      }
      val numLayers = maps.size
      val (width, height) = {
        import sys.process.*
        val Pattern = """\s*Image Width: (\d+) Image Length: (\d+)\s*""".r
        val res = s"tiffinfo ${layers(0).getAbsolutePath}".!!
        res.split("\n").map(_.trim).collectFirst {
          case Pattern(w, h) => (w.toInt, h.toInt)
        }.get
      }

      if (x * 64 * downsampling >= width || y * 64 * downsampling >= height || z * 64 * downsampling >= numLayers)
        throw new NoSuchElementException(s"Block $x $y $z q$downsampling is outside of volume bounds")

      writeBlock(target, downsampling) { (lx, ly, lz) =>
        val globalX = (x * 64 + lx) * downsampling
        val globalY = (y * 64 + ly) * downsampling
        val globalZ = (z * 64 + lz) * downsampling

        if (globalX >= 0 && globalX < width && globalY >= 0 && globalY < height && globalZ >= 0 && globalZ < numLayers) {
          val data = maps(globalZ)
          val offset = (globalY * width + globalX) * 2

          data.get(offset + 1) & bitmask
        } else 0
      }
    }
  }

  lazy val LayerCache = downloadUtils.downloadCache[(SegmentReference, Int)](
    { case (segment, layer) => segment.layerUrl(layer) },
    { case (segment, layer) => layerFile(segment, layer) },
    maxConcurrentRequests = config.maxConcurrentGridRequests,
    settings = CacheSettings.Default.copy(
      negTtlSeconds = 60,
      baseDirectory = Some(new File(config.dataDir, "grid")),
      maxCacheSize = config.gridCacheMaxSize,
      cacheHighWatermark = config.gridCacheHighWatermark,
      cacheLowWatermark = config.gridCacheLowWatermark
    )
  )

  def layerFile(segment: SegmentReference, layer: Int): File =
    new File(config.dataDir, f"grid/scroll${segment.scrollId}/segment/${segment.segmentId}/$layer%03d.tif")

  def layersForSegment(segment: SegmentReference): Future[Seq[File]] =
    Future.traverse(0 to 64)(z => LayerCache((segment, z)))

  def volumeLayersNeeded(z: Int, downsampling: Int): Seq[Int] =
    z * 64 * downsampling until (z + 1) * 64 * downsampling by downsampling

  def volumeLayersAvailableFor(scroll: ScrollReference, meta: VolumeMetadata, z: Int, downsampling: Int): Boolean =
    volumeLayersNeeded(z, downsampling).forall(VolumeLayerCache.isReady(scroll, meta, _))
  def volumeLayersForFragment(scroll: ScrollReference, meta: VolumeMetadata, z: Int, downsampling: Int): Future[Seq[MappedByteBuffer]] =
    Future.traverse(volumeLayersNeeded(z, downsampling))(layer => VolumeLayerCache((scroll, meta, layer)))

  def volumeBlock64x4IsAvailable(scroll: ScrollReference, meta: VolumeMetadata, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Boolean =
    VolumeBlockCache.isReady((scroll, meta, x, y, z, bitmask, downsampling))

  def volumeBlock64x4(scroll: ScrollReference, meta: VolumeMetadata, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] =
    VolumeBlockCache((scroll, meta, x, y, z, bitmask, downsampling))

  val VolumeBlockCache = downloadUtils.computeCache[(ScrollReference, VolumeMetadata, Int, Int, Int, Int, Int)](
    { case (scroll, meta, x, y, z, bitmask, downsampling) => new File(config.dataDir, f"blocks/scroll${scroll.scrollId}/${meta.uuid}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") },
    CacheSettings.Default.copy(negTtlSeconds = 60)
  ) {
      case (scroll, metadata, x, y, z, bitmask, downsampling) =>
        val target = new File(config.dataDir, f"blocks/scroll${scroll.scrollId}/${metadata.uuid}/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin") // FIXME: dry
        createVolumeBlock64x4FromLayers(scroll, metadata, target, x, y, z, bitmask, downsampling)
    }

  def volumeBlock64x4File(scroll: ScrollReference, uuid: String, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): File =
    new File(config.dataDir, f"blocks/scroll${scroll.scrollId}/$uuid/64-4/d$downsampling%02d/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b$bitmask%02x.bin")

  def createVolumeBlock64x4FromLayers(scroll: ScrollReference, meta: VolumeMetadata, target: File, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[File] =
    volumeLayersForFragment(scroll, meta, z, downsampling).flatMap { maps =>
      val numLayers = maps.size
      import meta.{ width, height }

      if (x * 64 * downsampling >= width || y * 64 * downsampling >= height)
        throw new NoSuchElementException(s"Block $x $y $z q$downsampling is outside of volume bounds")

      // try to pre-load the data
      // pages needed:
      //  - for each z layer
      //  - for each y row in question (y * 64 to (y + 1) * 64) * downsampling (base addr = y * width * 2)
      //  - Number of pages = (64 * downsampling * 2) / 4096, rounded up to next page
      //  -> 64 madvise calls for each layer

      val xOffset = x * 64 * downsampling * 2
      val numPages = (64 * downsampling * 2 + 4095) / 4096
      val yOffsets =
        for {
          y <- y * 64 until (y + 1) * 64
        } yield 8 + y * downsampling * width * 2

      val pages =
        for {
          yOffset <- yOffsets
          startOffset = yOffset + xOffset
          endOffset = startOffset + 64 * downsampling * 2
          //page <- (startOffset & ~4095) to (endOffset & ~4095) by 4096
        } yield ((startOffset & ~4095), (endOffset & ~4095) + 4096)

      /*pages.foreach {
        case (start, end) =>
          println(f"start: $start%8x end: $end%8x num: ${(end - start) / 4096}")
      }*/

      val coalesced =
        (pages.sortBy(_._1) :+ (Int.MaxValue, Int.MaxValue)).scanLeft(((-1, -1), Option.empty[(Int, Int)])) { (state, next) =>
          val (open, _) = state
          val (start1, end1) = open
          val (start2, end2) = next
          if (end1 > start2)
            println(f"Overlap: $start1%8x $end1%8x $start2%8x $end2%8x")

          val distance = (start2 - end1) / 4096
          if (distance <= MadviseCoalescePageGap) {
            //println(f"Coalescing: Distance between [$start1%8x-$end1%8x] to [$start2%8x-$end2%8x]: ${(start2 - end1) / 4096}")
            ((start1, end2), None) // coalesce and emit nothing
          } else ((start2, end2), Some(start1, end1)) // emit the previous range and continue with the current
        }.flatMap(_._2).drop(1)

      //coalesced.foreach(p => println(f"start: $p%8d end: ${p + numPages + 1}%8d num: ${numPages + 1} rowwidth: ${downsampling * width * 2}"))
      /*coalesced.foreach {
        case (start, end) =>
          println(f"start: $start%8x end: $end%8x num: ${(end - start) / 4096}")
      }*/

      val start = System.currentTimeMillis()
      for {
        map <- maps
        baseAddr = map.asInstanceOf[DirectBuffer].address() - 8
        (start, end) <- coalesced
      } {
        //val offset = (yOffset + xOffset) & ~4095 // align to page boundary
        //println(f"madvise $baseAddr%8x $offset%8x ${4096 * (numPages + 1)}")
        val res = Mmap.INSTANCE.madvise(baseAddr + start, end - start, Mmap.MADV_WILLNEED)
        if (res != 0)
          println(f"madvise failed for $baseAddr%8x $start%8x ${end - start}: $res")
      }
      println(s"Preloading took ${System.currentTimeMillis() - start}ms")

      val startWriting = System.currentTimeMillis()
      //val offs = new scala.collection.immutable.VectorBuilder[Int]()
      val res = writeBlock(target, downsampling) { (lx, ly, lz) =>
        val globalX = (x * 64 + lx) * downsampling
        val globalY = (y * 64 + ly) * downsampling
        //val globalZ = (z * 64 + lz) // we already downsample when selecting layers

        if (globalX >= 0 && globalX < width && globalY >= 0 && globalY < height && lz >= 0 && lz < numLayers) {
          val data = maps(lz)
          val offset = (globalY * width + globalX) * 2

          //offs += offset - 8
          data.get(offset + 1) & bitmask
        } else 0
      }

      res.onComplete { _ =>
        println(s"Writing block $x $y $z q$downsampling done after ${System.currentTimeMillis() - startWriting}ms")
        //offs.result().groupBy(_ & ~4095).toVector.sortBy(_._1).foreach(x => println(f"x: ${x._1}%8x num: ${x._2.size} distinct: ${x._2.distinct.size}"))
      }

      res
    }(blockingDispatcher)

  def createVolumeBlock64x4FromLayersToBytes(scroll: ScrollReference, meta: VolumeMetadata, x: Int, y: Int, z: Int, bitmask: Int, downsampling: Int): Future[Array[Byte]] =
    volumeLayersForFragment(scroll, meta, z, downsampling).flatMap { maps =>
      val numLayers = maps.size
      import meta.{ width, height }

      if (x * 64 * downsampling >= width || y * 64 * downsampling >= height)
        throw new NoSuchElementException(s"Block $x $y $z q$downsampling is outside of volume bounds")

      val res = blockBytes(downsampling) { (lx, ly, lz) =>
        val globalX = (x * 64 + lx) * downsampling
        val globalY = (y * 64 + ly) * downsampling
        //val globalZ = (z * 64 + lz) // we already downsample when selecting layers

        if (globalX >= 0 && globalX < width && globalY >= 0 && globalY < height && lz >= 0 && lz < numLayers) {
          val data = maps(lz)
          val offset = (globalY * width + globalX) * 2

          data.get(offset + 1) & bitmask
        } else 0
      }
      println(s"Generating block $x $y $z q$downsampling done")
      res
    }(blockingDispatcher)

  def volumeLayerFile(scroll: ScrollReference, meta: VolumeMetadata, layer: Int): File =
    new File(config.dataDir, f"grid/scroll${scroll.scrollId}/${meta.uuid}/${meta.formatLayer(layer)}.tif")

  lazy val VolumeLayerCache = downloadUtils.downloadCache[(ScrollReference, VolumeMetadata, Int)](
    { case (scroll, meta, layer) => f"${scroll.volumeUrl(meta.uuid)}${meta.formatLayer(layer)}.tif" },
    { case (scroll, meta, layer) => volumeLayerFile(scroll, meta, layer) },
    maxConcurrentRequests = config.maxConcurrentGridRequests,
    settings = CacheSettings.Default.copy(
      negTtlSeconds = 60,
      baseDirectory = Some(new File(config.dataDir, "grid")),
      maxCacheSize = config.gridCacheMaxSize,
      cacheHighWatermark = config.gridCacheHighWatermark,
      cacheLowWatermark = config.gridCacheLowWatermark
    )
  ).map { (info, file) =>
      info match {
        case (scroll, meta, layer) =>
          val raf = new java.io.RandomAccessFile(file, "r")
          val offset = meta.uuid match {
            case "20231117161658" => 368
            case _                => 8
          }
          val map = raf.getChannel.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, offset, raf.length() - offset)
          madviseSequential(map)
          map
      }
    }

  def madviseSequential(map: MappedByteBuffer): Unit = {
    val addr = map.asInstanceOf[DirectBuffer].address()
    Mmap.INSTANCE.madvise(addr, map.capacity(), Mmap.MADV_SEQUENTIAL)
  }
}
