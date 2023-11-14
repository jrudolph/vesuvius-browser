package net.virtualvoid.vesuvius
package tiles

import java.io.File

case class TilesConfig(
    host:                      String,
    port:                      Int,
    dataServerUsername:        String,
    dataServerPassword:        String,
    dataDir:                   File,
    maxConcurrentGridRequests: Int,
    maxGridCacheSize:          Long,
    requestsPerLayer:          Int
) extends DataServerConfig
object TilesConfig {
  def fromConfig(config: com.typesafe.config.Config): TilesConfig =
    TilesConfig(
      host = config.getString("app.host"),
      port = config.getInt("app.port"),
      dataServerUsername = config.getString("app.data-username"),
      dataServerPassword = config.getString("app.data-password"),
      dataDir = new File(config.getString("app.data-dir")),
      maxConcurrentGridRequests = config.getInt("app.max-concurrent-grid-requests"),
      maxGridCacheSize = config.getBytes("app.max-grid-cache-size"),
      requestsPerLayer = config.getInt("app.requests-per-layer")
    )
}