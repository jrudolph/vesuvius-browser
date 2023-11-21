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
    gridCacheMaxSize:          Long,
    gridCacheHighWatermark:    Double,
    gridCacheLowWatermark:     Double,
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
      gridCacheMaxSize = config.getBytes("app.grid-cache.max-size"),
      gridCacheHighWatermark = config.getDouble("app.grid-cache.high-watermark"),
      gridCacheLowWatermark = config.getDouble("app.grid-cache.low-watermark"),
      requestsPerLayer = config.getInt("app.requests-per-layer")
    )
}