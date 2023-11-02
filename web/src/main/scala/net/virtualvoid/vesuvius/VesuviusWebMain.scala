package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http

import scala.util.{ Failure, Success }

object VesuviusWebMain extends App {
  println(s"Booting up Vesuvius web server version ${BuildInfo.version} built at ${BuildInfo.builtAtString}")

  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  val appConfig = AppConfig.fromConfig(system.settings.config)

  val routes = new VesuviusRoutes(appConfig).main

  val server = Http().newServerAt(appConfig.host, appConfig.port).bind(routes)
  server.onComplete {
    case Success(s) =>
      println(s"Server started on http:/${s.localAddress}")
    case Failure(ex) =>
      println(s"Server could not be started: $ex")
      system.terminate()
  }
}
