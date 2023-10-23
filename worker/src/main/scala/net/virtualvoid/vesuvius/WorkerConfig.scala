package net.virtualvoid.vesuvius

import com.typesafe.config.Config

import java.io.File

case class WorkerConfig(
                         dataServerUsername: String,
                         dataServerPassword: String,
                         dataDir: File,
                       )

object WorkerConfig {
  def fromConfig(config: Config): WorkerConfig = {
    WorkerConfig(
      dataServerUsername = config.getString("app.data-username"),
      dataServerPassword = config.getString("app.data-password"),
      dataDir = new File(config.getString("app.data-dir")),
    )
  }
}