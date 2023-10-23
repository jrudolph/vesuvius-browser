package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem

object VesuviusWorkerMain extends App {
  implicit val system: ActorSystem = ActorSystem()
  import system.dispatcher

  val appConfig = WorkerConfig.fromConfig(system.settings.config)
}
