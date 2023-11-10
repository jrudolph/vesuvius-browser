package net.virtualvoid.vesuvius

import com.typesafe.config.Config

import java.io.File

case class AppConfig(
    host:               String,
    port:               Int,
    dataServerUsername: String,
    dataServerPassword: String,
    adminPassword:      String,
    serverKey:          String,
    dataDir:            File,
    concurrentResizes:  Int
) extends DataServerConfig

object AppConfig {
  def fromConfig(config: Config): AppConfig = {
    AppConfig(
      host = config.getString("app.host"),
      port = config.getInt("app.port"),
      dataServerUsername = config.getString("app.data-username"),
      dataServerPassword = config.getString("app.data-password"),
      adminPassword = config.getString("app.admin-password"),
      serverKey = config.getString("app.server-key"),
      dataDir = new File(config.getString("app.data-dir")),
      concurrentResizes = config.getInt("app.concurrent-resizes")
    )
  }
}