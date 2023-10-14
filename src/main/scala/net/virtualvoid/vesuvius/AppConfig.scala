package net.virtualvoid.vesuvius

import com.typesafe.config.Config

case class AppConfig(
                      host:  String,
                      port:  Int,
                    )

object AppConfig {
  def fromConfig(config: Config): AppConfig = {
    AppConfig(
      host = config.getString("app.host"),
      port = config.getInt("app.port"),
    )
  }
}