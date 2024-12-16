package net.virtualvoid.vesuvius

import org.apache.pekko.http.scaladsl.model.DateTime
import org.apache.pekko.http.scaladsl.server.*
import org.apache.pekko.http.scaladsl.server.util.Tupler

import java.io.File
import scala.concurrent.Future

trait PekkoHttpHelpers {
  import Directives.*

  implicit class FutureExtension[T](val future: Future[T]) {
    def await: Directive1[T] = onSuccess(future)
  }
  implicit class OptionDirectiveExtension[T](dir: Directive1[Option[T]]) {
    def orReject: Directive1[T] = dir.flatMap {
      case Some(t) => provide(t)
      case None    => reject
    }
  }
  implicit class FileDirectiveExtension[T](directive: Directive1[File]) {
    def deliver: Route = directive { file =>
      conditional(DateTime(file.lastModified())) {
        getFromFile(file)
      }
    }
  }
  implicit class FutureFileDirectiveExtension[T](f: Future[File]) {
    def deliver: Route = f.await.deliver
  }
}
