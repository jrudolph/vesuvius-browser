package net.virtualvoid.vesuvius.tiles

import org.apache.pekko.actor.ActorSystem

import java.awt.Color
import java.io.*
import java.nio.channels.FileChannel.MapMode
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.HashMap
import scala.concurrent.Future

trait Volume {
  def valueAt(x: Int, y: Int, z: Int): Int
}

object Volume {
  def fromBlocks(cacheDir: File, downsampled: Int)(implicit system: ActorSystem): Volume = {
    val states = new AtomicReference(HashMap.empty[(Int, Int, Int), BlockState])

    def fileFor(x: Int, y: Int, z: Int): File =
      new File(f"$cacheDir/z$z%03d/xyz-$x%03d-$y%03d-$z%03d-b255-d${downsampled}%02d.bin")

    def tileFor(x: Int, y: Int, z: Int): (Int, Int, Int) =
      (x / 64, y / 64, z / 64)

    def loadTile(x: Int, y: Int, z: Int): (Int, Int, Int) => Int =
      states.get().get((x, y, z)) match {
        case Some(Loaded(map)) =>
          (x: Int, y: Int, z: Int) =>
            val xBlock = x >> 2
            val yBlock = y >> 2
            val zBlock = z >> 2

            val block = zBlock * 256 + yBlock * 16 + xBlock
            val blockX = x & 0x3
            val blockY = y & 0x3
            val blockZ = z & 0x3

            val off = block * 64 + blockZ * 16 + blockY * 4 + blockX
            map.get(off) & 0xff
        case None =>
          val file = fileFor(x, y, z)
          println(s"Loading $file")
          require(file.exists(), s"Missing block file $file")
          val raf = new RandomAccessFile(file, "r")
          val channel = raf.getChannel
          val map = channel.map(MapMode.READ_ONLY, 0, channel.size())
          val oldState = states.get()
          val newState = states.get().updated((x, y, z), Loaded(map))
          states.compareAndSet(oldState, newState)
          loadTile(x, y, z)
      }

    new Volume {
      override def valueAt(x: Int, y: Int, z: Int): Int = {
        val (tx, ty, tz) = tileFor(x, y, z)
        val lx = x & 0x3f
        val ly = y & 0x3f
        val lz = z & 0x3f
        loadTile(tx, ty, tz)(lx, ly, lz)
      }
    }
  }

  sealed trait BlockState
  case object Unknown extends BlockState
  case class Loaded(map: java.nio.ByteBuffer) extends BlockState
  case object Failed extends BlockState
  case class Downloading(future: Future[Loaded]) extends BlockState
  //case class TryLater(atNanos: Long) extends BlockState

}

object CreateVerticalStack extends App {
  //val center = (4525, 2868, 8955)
  //val center = (3986 / 4, 3082 / 4, 11298 / 4)
  //val center = (4019 / 2, 3244 / 2, 9187 / 2)
  val downsampled = 1
  val center = (4534 / downsampled, 2238 / downsampled, 5143 / downsampled)
  val cacheDir = f"/home/johannes/git/self/_2023/vesuvius-gui/data4/scroll1667/20231107190228/64-4/d${downsampled}%02d/"
  implicit val system: ActorSystem = ActorSystem("vesuvius")
  val volume = Volume.fromBlocks(new File(cacheDir), downsampled)
  println(volume.valueAt(center._1, center._2, center._3))

  def p(x: Int, y: Int, z: Int): Int = volume.valueAt(x, y, z)

  val radius = 50
  val zRadius = 40
  val Threshold = 110

  val xSpan = (center._1 - radius) until (center._1 + radius)
  val ySpan = (center._2 - radius) until (center._2 + radius)
  val zSpan = (center._3 - zRadius) until (center._3 + zRadius)

  val x0 = xSpan.head
  val y0 = ySpan.head
  val z0 = zSpan.head

  val voxFile = new File("new6.vox")

  /*  def genSlice(): Unit = {
    for {
      y <- ySpan

    } {
      val imagePath = f"/home/johannes/tmp/pap/20230827161847/vertical-stack/$y%04d.png"
      val image = new BufferedImage(xSpan.size, zSpan.size, java.awt.image.BufferedImage.TYPE_INT_RGB)
      for {
        x <- xSpan
        z <- zSpan
      } {
        val pixel = p(x, y, z)
        //image.setRGB(z, y, pixel << 8 | pixel)
        // convert from 16bit gray to 8bit rgb
        val v = (pixel >> 8) & 0xff
        image.setRGB(x - xSpan.head, z, v << 16 | v << 8 | v)
      }
      require(ImageIO.write(image, "png", new java.io.File(imagePath)))

      println(s"Wrote $imagePath")
    }
  }*/

  def readVox(): Unit = {
    val f = voxFile
    val fis = new FileInputStream(f)
    def tag(): String =
      new String(Array.fill(4)(fis.read().toByte), "ASCII")
    def uintle32(): Int =
      fis.read() | fis.read() << 8 | fis.read() << 16 | fis.read() << 24

    def string(): String = {
      val len = uintle32()
      val buf = new Array[Byte](len)
      val read = fis.read(buf)
      require(read == buf.length)
      new String(buf, "ASCII")
    }
    def dict(): Seq[(String, String)] = {
      val numEntries = uintle32()
      Vector.fill(numEntries)((string(), string()))
    }

    require(tag() == "VOX ")
    require(uintle32() == 200)

    def readTag(indent: String): Unit = {
      val t = tag()
      val contentSize = uintle32()
      val childrenSize = uintle32()
      println(s"${indent}Tag: $t contentSize: $contentSize childrenSize: $childrenSize")
      t match {
        case "nTRN" =>
          val nodeId = uintle32()
          val d = dict()
          val childId = uintle32()
          val reserved = uintle32()
          val layer = uintle32()
          val numFrames = uintle32()
          val frames = Vector.fill(numFrames)(dict())
          println(s"${indent}  nodeId: $nodeId childId: $childId layer: $layer numFrames: $numFrames d: $d frames: $frames")
        case "nGRP" =>
          val nodeId = uintle32()
          val d = dict()
          val numChildren = uintle32()
          val children = Vector.fill(numChildren)(uintle32())
          println(s"${indent}  nodeId: $nodeId numChildren: $numChildren d: $d children: $children")
        case "nSHP" =>
          val nodeId = uintle32()
          val d = dict()
          val numModels = uintle32()
          val models = Vector.fill(numModels)(uintle32() -> dict())
          println(s"${indent}  nodeId: $nodeId numModels: $numModels d: $d models: $models")
        case "MATL" =>
          val id = uintle32()
          val d = dict()
          println(s"${indent}  id: $id d: $d")
        case _ =>
          val buf = new Array[Byte](contentSize)
          val read = fis.read(buf)
          require(read == buf.length)
      }

      val start = fis.getChannel.position()
      val end = start + childrenSize

      while (fis.getChannel.position() < end) {
        println(s"${fis.getChannel.position()} -> ${end} $contentSize $childrenSize $start rem: ${end - fis.getChannel.position()}")
        readTag(indent + "  ")
      }
      println(s"${indent}end <$t>")
    }
    readTag("")
  }

  def threshold(v: Int): Boolean =
    v > Threshold

  sealed trait TransformChild {
    def nodeId: Int
  }
  case class TransformNode(nodeId: Int, frame0Dict: Seq[(String, String)], child: TransformChild)
  case class GroupNode(nodeId: Int, children: Seq[TransformNode]) extends TransformChild
  case class ShapeNode(nodeId: Int, models: Seq[Int]) extends TransformChild

  def genVox(): Unit = {
    val voxels = for {
      x <- xSpan
      y <- ySpan
      z <- zSpan
      v = p(x, y, z)
      if threshold(v)
    } yield (x - x0, y - y0, z - z0, v)

    //voxels.take(100).foreach(println)

    val f = voxFile
    f.delete()
    val raf = new RandomAccessFile(f, "rw") //new BufferedOutputStream(new FileOutputStream("out3.vox"))
    val out = new BufferedOutputStream(new FileOutputStream(raf.getFD))

    def tag(t: String): Unit = {
      val data = t.getBytes("ASCII")
      require(data.length == 4)
      out.write(data)
    }
    def uint32le(v: Int): Unit = {
      out.write(v & 0xff)
      out.write((v >> 8) & 0xff)
      out.write((v >> 16) & 0xff)
      out.write((v >> 24) & 0xff)
    }
    def curPos(): Long = {
      out.flush()
      raf.getFilePointer
    }
    def withChildrenLength(f: => Unit): Unit = {
      val pos = curPos()
      uint32le(0) // placeholder
      f
      val newPos = curPos()
      val length = (newPos - pos - 4).toInt
      raf.seek(pos)
      uint32le(length)
      out.flush()
      raf.seek(newPos)
    }

    def withOwnLength(f: => Unit): Unit = {
      val pos = curPos()
      uint32le(0) // placeholder
      f
      val newPos = curPos()
      val length = (newPos - pos - 8).toInt
      raf.seek(pos)
      uint32le(length)
      out.flush()
      raf.seek(newPos)
    }

    tag("VOX ")
    uint32le(200)
    tag("MAIN")
    uint32le(0) // MAIN content size

    withChildrenLength {
      val groups = voxels.groupBy { case (x, y, z, v) => (x / 256, y / 256) }.toVector
      groups
        //.take(1)
        .foreach {
          case ((xt, yt), voxels) =>
            tag("SIZE")
            uint32le(12) // SIZE content size
            uint32le(0) // SIZE children size
            uint32le(256)
            uint32le(256)
            uint32le(zSpan.size)

            tag("XYZI")
            val size = 4 + 4 * voxels.size
            println(f"XYZI size: $size for tile $xt/$yt numVoxels: ${voxels.size}")
            uint32le(size) // XYZI content size
            uint32le(0) // XYZI children size
            uint32le(voxels.size)
            for ((xi, yi, z, v) <- voxels) {
              val x = xi - xt * 256
              val y = yi - yt * 256
              //println(f"XYZI: $x $y $z $v")
              val xyzi = x | y << 8 | z << 16 | v << 24
              uint32le(xyzi)
            }
        }

      val groupNodes =
        groups.zipWithIndex.map {
          case (((x, y), _), i) =>
            TransformNode(2 + i * 2, Seq("_t" -> s"${x * 256} ${y * 256} 0"), ShapeNode(3 + i * 2, Vector(i)))
        }

      val structure =
        TransformNode(
          0,
          Nil,
          GroupNode(
            1,
            groupNodes
          )
        )

      def string(str: String): Unit = {
        val data = str.getBytes("ASCII")
        uint32le(data.length)
        out.write(data)
      }
      def dict(es: Seq[(String, String)]): Unit = {
        uint32le(es.size)
        for ((k, v) <- es) {
          string(k)
          string(v)
        }
      }

      def render(tn: TransformNode): Unit = {
        tag("nTRN")
        withOwnLength {
          uint32le(0)
          uint32le(tn.nodeId)
          dict(Nil)
          uint32le(tn.child.nodeId)
          uint32le(-1) // reserved
          uint32le(0) // layer 0
          uint32le(1) // num frames
          dict(tn.frame0Dict)
        }

        tn.child match {
          case gn: GroupNode =>
            tag("nGRP")
            withOwnLength {
              uint32le(0)
              uint32le(gn.nodeId)
              dict(Nil)
              uint32le(gn.children.size)
              gn.children.foreach(c => uint32le(c.nodeId))
            }
            gn.children.foreach(render)
          case sn: ShapeNode =>
            tag("nSHP")
            withOwnLength {
              uint32le(0)
              uint32le(sn.nodeId)
              dict(Nil)
              uint32le(sn.models.size)
              sn.models.foreach { modelId =>
                uint32le(modelId)
                dict(Nil)
              }
            }
        }
      }

      render(structure)

      tag("RGBA")
      uint32le(256 * 4) // RGBA content size
      uint32le(0) // RGBA children size
      for (i <- 0 until 256) {
        val v = i // (i - Threshold) * 255 / Threshold
        val exp = 0.7f
        val rgb = Color.HSBtoRGB((56f / 360 * math.pow(v.toFloat / 255, exp)).toFloat, 1f - 0.5f * math.pow(v.toFloat / 255, exp).toFloat, 1)

        // start 0
        // end 53 / 360

        val r = (rgb >> 16) & 0xff
        val g = (rgb >> 8) & 0xff
        val b = (rgb >> 0) & 0xff
        uint32le(r << 0 | g << 8 | b << 16 | v << 24)
      }

      // Tag: MATL contentSize: 133 childrenSize: 0
      //[info]     id: 148 d: Vector((_type,_glass), (_alpha,0.385473), (_trans,0.385473), (_rough,0.1), (_ior,0.05), (_ri,1.05), (_d,0.05))
      //[info]   end <MATL>

      for (i <- 0 until 256) {
        val v = (i - Threshold) * 255 / Threshold

        tag("MATL")
        withOwnLength {
          uint32le(0)
          uint32le(i)
          dict(
            Seq(
              "_type" -> "_glass",
              "_media_type" -> "_scatter",
              "_alpha" -> (1f - v.toFloat / 255).toString,
              "_trans" -> (1f - v.toFloat / 255).toString,
              "_rough" -> "0.1",
              "_ior" -> "0.00",
              "_ri" -> "1.00", //.05",
              "_d" -> "0.05"
            )
          )
        }
      }
    }

    out.close()
    raf.close()
  }

  //readVox()
  genVox()
  readVox()
  sys.exit()
}
