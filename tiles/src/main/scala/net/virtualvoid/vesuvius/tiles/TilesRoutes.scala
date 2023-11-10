package net.virtualvoid.vesuvius
package tiles

import net.virtualvoid.vesuvius.ScrollReference
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

class TilesRoutes(config: TilesConfig) {
  lazy val main: Route =
    tilesRoutes

  lazy val tilesRoutes =
    pathPrefix("tiles") {
      pathPrefix("scroll" / Scroll) { scroll =>
        complete(s"Hello $scroll")
      }
    }

  lazy val Scroll = IntNumber.flatMap(ScrollReference.byId)
}
