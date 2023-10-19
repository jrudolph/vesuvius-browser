package net.virtualvoid.vesuvius

import java.awt.image.BufferedImage
import java.io.{ BufferedOutputStream, FileOutputStream, RandomAccessFile }
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

  val xSpan = 3100 until 3350 //3000 to 3500
  val ySpan = 4400 until 4630
  val zSpan = 0 to 64

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

  def threshold(v: Int): Boolean =
    v > 32000

  val voxels = for {
    x <- xSpan
    y <- ySpan
    z <- zSpan
    v = p(x, y, z)
    if threshold(v)
  } yield (x - x0, y - y0, z - z0, v)

  def genVox(): Unit = {
    val out = new BufferedOutputStream(new FileOutputStream("out2.vox"))

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

    tag("VOX ")
    uint32le(200)
    tag("MAIN")
    uint32le(0) // MAIN content size
    uint32le(voxels.size * 4 + 10 * 4 + 259 * 4) // MAIN children size

    tag("SIZE")
    uint32le(12) // SIZE content size
    uint32le(0) // SIZE children size
    uint32le(xSpan.size)
    uint32le(ySpan.size)
    uint32le(zSpan.size)

    tag("XYZI")
    uint32le(4 + 4 * voxels.size) // XYZI content size
    uint32le(0) // XYZI children size
    uint32le(voxels.size)
    for ((x, y, z, v) <- voxels) {
      val xyzi = x | y << 8 | z << 16 | (v >> 8) << 24
      uint32le(xyzi)
    }

    tag("RGBA")
    uint32le(256 * 4) // RGBA content size
    uint32le(0) // RGBA children size
    for (i <- 0 until 256) {
      val v = 255 - i
      uint32le(v << 0 | v << 8 | v << 16 | 0xff << 24)
    }

    out.close()
  }

  genVox()
}
