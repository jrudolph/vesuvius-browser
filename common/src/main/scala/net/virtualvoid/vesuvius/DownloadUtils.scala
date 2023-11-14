package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ HttpMethods, HttpRequest, StatusCodes, headers }
import org.apache.pekko.stream.scaladsl.{ FileIO, Sink, Source }

import java.io.File
import scala.concurrent.{ Future, Promise }

trait DataServerConfig {
  def dataServerUsername: String
  def dataServerPassword: String
}

class DownloadUtils(config: DataServerConfig)(implicit system: ActorSystem) {
  import system.dispatcher

  val DefaultPositiveTtl = 3600 * 24 * 365
  val DefaultNegativeTtl = 7200

  def computeCache[T](filePattern: T => File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl, isValid: File => Boolean = _ => true)(compute: T => Future[File]): T => Future[File] = {
    val cache = LfuCache[T, File]
    t => cache.getOrLoad(t, _ => cached(filePattern(t), ttlSeconds, negTtlSeconds, isValid)(() => compute(t)))
  }

  def downloadCache[T](urlPattern: T => String, filePattern: T => File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl, isValid: File => Boolean = _ => true, maxConcurrentRequests: Int = 16): T => Future[File] = {
    val limited = semaphore[T, File](maxConcurrentRequests)(t => download(urlPattern(t), filePattern(t)))
    computeCache(filePattern, ttlSeconds, negTtlSeconds, isValid)(limited)
  }

  def cacheDownload(url: String, to: File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl): Future[File] =
    cached(to, ttlSeconds, negTtlSeconds) { () => download(url, to) }
  def cached(to: File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl, isValid: File => Boolean = _ => true)(f: () => Future[File]): Future[File] = {
    to.getParentFile.mkdirs()
    val neg = new File(to.getParentFile, s".neg-${to.getName}")
    if (to.exists() && to.lastModified() + ttlSeconds * 1000 > System.currentTimeMillis() && isValid(to)) Future.successful(to)
    else if (neg.exists() && neg.lastModified() + negTtlSeconds * 1000 > System.currentTimeMillis() && isValid(neg)) Future.failed(new RuntimeException(s"Negatively cached"))
    else
      f().recoverWith {
        case t: Throwable =>
          neg.getParentFile.mkdirs()
          neg.delete()
          neg.createNewFile()
          Future.failed(t)
      }
  }

  val auth = headers.Authorization(headers.BasicHttpCredentials(config.dataServerUsername, config.dataServerPassword))
  def download(url: String, to: File): Future[File] = {
    println(s"Downloading $url")
    val tmpFile = new File(to.getParentFile, s".tmp-${to.getName}")
    Http().singleRequest(HttpRequest(HttpMethods.GET, uri = url, headers = auth :: Nil))
      .flatMap { res =>
        require(res.status == StatusCodes.OK, s"Got status ${res.status} for $url")
        res.entity.dataBytes.runWith(FileIO.toPath(tmpFile.toPath))
      }
      .map { _ => tmpFile.renameTo(to); to }
  }

  def semaphore[T, U](numRequests: Int, queueLength: Int = 1000)(f: T => Future[U]): T => Future[U] = {
    val queue =
      Source.queue[(T, Promise[U])](queueLength)
        .mapAsyncUnordered(numRequests) {
          case (t, promise) =>
            f(t).onComplete(promise.complete)
            promise.future
        }
        .to(Sink.ignore)
        .run()

    t => {
      val promise = Promise[U]()
      queue.offer(t -> promise)
      promise.future
    }
  }
}
