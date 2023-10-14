package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.model.{ StatusCodes, Uri }
import org.apache.pekko.http.scaladsl.server.{ Directive1, Directives, Route }

import scala.concurrent.Future

class VesuviusRoutes()(implicit system: ActorSystem) extends Directives with TwirlSupport {
  import system.dispatcher

  lazy val main = encodeResponse(mainRoute)

  lazy val mainRoute =
    concat(
      getFromResourceDirectory("web")
    )
}
