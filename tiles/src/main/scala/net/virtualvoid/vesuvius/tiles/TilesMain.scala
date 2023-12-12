package net.virtualvoid.vesuvius
package tiles

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http

import scala.util.{ Failure, Success }

object TilesMain extends App {
  println(s"Booting up Vesuvius tiles server version ${BuildInfo.version} built at ${BuildInfo.builtAtString}")

  implicit val system: ActorSystem = ActorSystem()

  import system.dispatcher

  val config = TilesConfig.fromConfig(system.settings.config)

  val routes = new TilesRoutes(config).main

  Http().newServerAt(config.host, config.port).bind(routes)
    .onComplete {
      case Success(s) =>
        println(s"Server started on ${s.localAddress}")
      case Failure(ex) =>
        println(s"Server could not be started: $ex")
        system.terminate()
    }
}
