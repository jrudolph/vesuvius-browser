package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import spray.json._

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
    voxelsize: Double) {
  def formatLayer(layer: Int): String =
    if (slices >= 10000) f"$layer%05d" else f"$layer%04d"

  def energykeV: Int = VolumeMetadata.EnergyRegex.findFirstMatchIn(name).map(_.group(1).toInt).getOrElse(0)
}
object VolumeMetadata {
  import DefaultJsonProtocol.*
  implicit val format: RootJsonFormat[VolumeMetadata] = jsonFormat9(apply)

  private[VolumeMetadata] val EnergyRegex = """(\d+)\s*keV""".r
}

class VolumeMetadataRepository(downloadUtils: DownloadUtils, dataDir: File)(implicit system: ActorSystem) {
  import system.dispatcher

  private val MetadataCache = downloadUtils.downloadCache[(ScrollReference, String)](
    { case (scroll, volumeId) => scroll.volumeMetadataUrl(volumeId) },
    { case (scroll, volumeId) => new File(dataDir, s"metadata/${scroll.scrollId}-${volumeId}.json") }
  )

  def metadataForVolume(scroll: ScrollReference, volumeId: String): Future[VolumeMetadata] =
    MetadataCache((scroll, volumeId))
      .map { metadata =>
        scala.io.Source.fromFile(metadata).mkString.parseJson.convertTo[VolumeMetadata]
      }
}
