package net.virtualvoid.vesuvius

import com.typesafe.config.Config

import java.io.File

case class WorkerConfig(
    workerId:            String,
    workEndpoint:        String,
    dataServerUsername:  String,
    dataServerPassword:  String,
    dataDir:             File,
    inferenceScriptDir:  File,
    concurrentDownloads: Int,
    supportedWorkTypes:  Seq[String]
)

object WorkerConfig {
  def fromConfig(config: Config): WorkerConfig = {
    WorkerConfig(
      workerId = config.getString("app.worker-id"),
      workEndpoint = config.getString("app.work-endpoint"),
      dataServerUsername = config.getString("app.data-username"),
      dataServerPassword = config.getString("app.data-password"),
      dataDir = new File(config.getString("app.data-dir")),
      inferenceScriptDir = new File(config.getString("app.inference-script-dir")),
      concurrentDownloads = config.getInt("app.concurrent-downloads"),
      supportedWorkTypes = config.getString("app.work-types").split(",").map(_.trim).toVector.map(parseWorkType)
    )
  }

  def parseWorkType: String => String = {
    case "inference"      => InferenceWorkItemInput("", null, InferenceParameters(0, 0, false)).productPrefix
    case "fingerprint"    => PPMFingerprintWorkItemInput.productPrefix
    case "downsample_ppm" => DownsamplePPMWorkItemInput("", 0).productPrefix
  }
}
