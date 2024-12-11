package net.virtualvoid.vesuvius

import java.awt.{ Color, RenderingHints }
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object SegmentOutline {
  def generate(crosscutReport: SegmentCrosscutReport, width: Int, height: Int, volumeWidth: Int, volumeHeight: Int, targetFile: File): Unit = {
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = bufferedImage.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)

    crosscutReport.crosscuts.sortBy(_.z).foreach { cut =>
      val c = Color.getHSBColor(cut.z / 12000f, 1f, 0.8f)
      g.setColor(new Color(c.getRed, c.getGreen, c.getBlue, 60))
      cut.lines.foreach {
        case CrosscutLine(Point2D(x1, y1), Point2D(x2, y2)) =>
          val x1i = (x1 * width / volumeWidth)
          val y1i = (y1 * height / volumeHeight)
          val x2i = (x2 * width / volumeWidth)
          val y2i = (y2 * height / volumeHeight)
          g.drawLine(x1i, y1i, x2i, y2i)
      }
    }

    ImageIO.write(bufferedImage, "png", targetFile)
  }
}
