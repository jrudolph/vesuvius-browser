package net.virtualvoid.vesuvius

import org.apache.pekko.http.scaladsl.model.{ HttpEntity, HttpResponse, MediaTypes, ResponseEntity }

object ImageTools {
  val EmptyImage: ResponseEntity = {
    val emptyImage = new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_RGB)
    val g = emptyImage.getGraphics
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, 1, 1)
    g.dispose()

    val baos = new java.io.ByteArrayOutputStream
    javax.imageio.ImageIO.write(emptyImage, "png", baos)
    HttpEntity(MediaTypes.`image/png`, baos.toByteArray)
  }
  val EmptyImageResponse: HttpResponse = HttpResponse(entity = EmptyImage)
}
