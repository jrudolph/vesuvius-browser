package net.virtualvoid.vesuvius

import java.awt.image.BufferedImage
import java.io.{ BufferedOutputStream, File, FileInputStream, FileOutputStream, RandomAccessFile }
import java.nio.ByteOrder
import java.nio.channels.FileChannel.MapMode
import javax.imageio.ImageIO

trait Image {
  def getPixel(x: Int, y: Int): Int
}
object Image {
  def fromTiff(path: String, width: Int, height: Int): Image = {
    // FIXME: we assume little endian, 16bit with data starting at byte 8
    // better validate using tiffinfo that this is correct
    val f = new RandomAccessFile(path, "r")
    val map = f.getChannel.map(MapMode.READ_ONLY, 8, f.length() - 8)
    val buf = map.order(ByteOrder.LITTLE_ENDIAN).asShortBuffer()

    new Image {
      override def getPixel(x: Int, y: Int): Int =
        (buf.get(y * width + x) & 0xffff)
    }
  }
}

object CreateVerticalStack extends App {
  def layerPath(layer: Int): String =
    f"/home/johannes/tmp/pap/20230827161847/layers/$layer%02d.tif"

  val width = 5048
  val height = 9163

  val stack = (0 to 64).map(layerPath).map(Image.fromTiff(_, width, height))

  def p(x: Int, y: Int, z: Int): Int = stack(z).getPixel(x, y)

  val xSpan = 2670 until (2670 + 530)
  val ySpan = 3955 until (3953 + 700)
  //val xSpan = 3100 until 3350
  //val ySpan = 4400 until 4630
  val zSpan = 0 to 58

  val x0 = xSpan.head
  val y0 = ySpan.head
  val z0 = zSpan.head

  def genSlice(): Unit = {
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
  }

  def readVox(): Unit = {
    val f = new File("out3.vox")
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
    v > 35000

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

    val f = new File("out3.vox")
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
              val xyzi = x | y << 8 | z << 16 | (v >> 8) << 24
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
        val v = 255 - i
        uint32le(v << 0 | v << 8 | v << 16 | 0xff << 24)
      }
    }

    out.close()
    raf.close()
  }

  genVox()
  readVox()
}
