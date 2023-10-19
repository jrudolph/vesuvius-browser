package net.virtualvoid.vesuvius

import java.awt.image.BufferedImage
import java.io.RandomAccessFile
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
        buf.get(y * width + x)
    }
  }
}

object CreateVerticalStack extends App {
  def layerPath(layer: Int): String =
    f"/tmp/pap/20230827161847/layers/$layer%02d.tif"

  val width = 5048
  val height = 9163

  val stack = (0 to 64).map(layerPath).map(Image.fromTiff(_, width, height))

  def p(x: Int, y: Int, z: Int): Int = stack(z).getPixel(x, y)

  val xSpan = 3000 to 3500
  val ySpan = 4400 to 4630
  val zSpan = 0 to 64

  for {
    y <- ySpan

  } {
    val imagePath = f"/tmp/pap/20230827161847/vertical-stack/$y%04d.png"
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
