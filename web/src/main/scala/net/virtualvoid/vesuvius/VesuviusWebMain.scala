package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http

import scala.concurrent.Future
import scala.util.{ Failure, Success }

object VesuviusWebMain extends App {
  println(s"Booting up Vesuvius web server version ${BuildInfo.version} built at ${BuildInfo.builtAtString}")

  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  val appConfig = AppConfig.fromConfig(system.settings.config)

  val routes = new VesuviusRoutes(appConfig).main

  val ports = Seq(appConfig.port, appConfig.port + 1)
  // run two servers on adjacent port to allow different K8s services with different configs
  val servers = Future.traverse(ports)(port => Http().newServerAt(appConfig.host, port).bind(routes))
  servers.onComplete {
    case Success(s) =>
      println(s"Servers started on ${s.map(_.localAddress).mkString(", ")}")
    case Failure(ex) =>
      println(s"Server could not be started: $ex")
      system.terminate()
  }
}
