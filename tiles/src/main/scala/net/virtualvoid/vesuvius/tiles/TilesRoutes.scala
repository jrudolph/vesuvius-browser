package net.virtualvoid.vesuvius
package tiles

import net.virtualvoid.vesuvius.ScrollReference
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import spray.json.*

import java.io.File
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
  import DefaultJsonProtocol._
  implicit val format: RootJsonFormat[VolumeMetadata] = jsonFormat9(apply)
}

class TilesRoutes(config: TilesConfig)(implicit system: ActorSystem) extends SprayJsonSupport {
  import system.dispatcher
  val downloadUtils = new DownloadUtils(config)

  lazy val main: Route =
    tilesRoutes

  lazy val tilesRoutes =
    pathPrefix("tiles") {
      pathPrefix("scroll" / Scroll) { scroll =>
        metadataForVolume(scroll).await { meta =>
          complete(s"Hello $meta")
        }
      }
    }

  lazy val Scroll = IntNumber.flatMap(ScrollReference.byId)

  def metadataForVolume(scroll: ScrollReference): Future[VolumeMetadata] =
    downloadUtils.cacheDownload(
      scroll.volumeMetadataUrl,
      new File(config.dataDir, s"metadata/${scroll.scroll}-${scroll.volumeId}.json")
    ).map { metadata =>
        scala.io.Source.fromFile(metadata).mkString.parseJson.convertTo[VolumeMetadata]
      }
}
