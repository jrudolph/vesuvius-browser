package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.model.{ StatusCodes, Uri }
import org.apache.pekko.http.scaladsl.server.{ Directive1, Directives, PathMatchers, Route }
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString

import java.io.File
import scala.concurrent.Future

class VesuviusRoutes()(implicit system: ActorSystem) extends Directives with TwirlSupport {
  import system.dispatcher

  val dataDir = new File("data")

  lazy val main = encodeResponse(mainRoute)

  val NameR = """(\d+)_(\d+).webp""".r
  val imageSize = (3902, 4834)

  lazy val mainRoute =
    concat(
      pathSingleSlash {
        get {
          complete {
            html.home()
          }
        }
      },
      path("image_files" / IntNumber / Segment) { (layer, name) =>
        val NameR(xStr, yStr) = name
        val x = xStr.toInt
        val y = yStr.toInt

        val resized = resize("/tmp/20230902141231.tif", layer, x, y)

        getFromFile(resized)
      },
      getFromResourceDirectory("web")
    )

  def resize(imageFile: String, layer: Int, tileX: Int, tileY: Int): File = {
    val maxLayer = (math.log(imageSize._1.max(imageSize._2)) / math.log(2)).ceil.toInt
    val size = 1 << (maxLayer - layer)
    val targetX = tileX * size * 256
    val targetY = tileY * size * 256

    val targetFile = new File(dataDir, s"$layer/${tileX}_$tileY.webp")
    if (targetFile.exists()) targetFile
    else {
      targetFile.getParentFile.mkdirs()
      val width = (imageSize._1 / size).min(256)
      val height = (imageSize._2 / size).min(256)

      import sys.process._
      val tmpFile = new File(dataDir, s"$layer/.tmp-${tileX}_$tileY.webp")

      val cmd = s"""convert $imageFile -crop ${size * width}x${size * height}+$targetX+$targetY -resize ${width}x$height -define webp:lossless=true $tmpFile"""
      println(f"Command for $layer/$tileX/$tileY: $cmd")
      cmd.!!

      tmpFile.renameTo(targetFile)
      targetFile
    }
  }
}
