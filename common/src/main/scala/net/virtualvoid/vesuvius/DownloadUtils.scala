package net.virtualvoid.vesuvius

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.caching.scaladsl.{ CachingSettings, LfuCacheSettings }
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ HttpMethods, HttpRequest, StatusCodes, headers }
import org.apache.pekko.stream.scaladsl.{ FileIO, Sink, Source }

import java.io.File
import scala.concurrent.{ Future, Promise }
import scala.concurrent.duration.*

trait DataServerConfig {
  def dataServerUsername: String
  def dataServerPassword: String
}

case class CacheSettings(
    ttlSeconds:    Long,
    negTtlSeconds: Long,
    isValid:       File => Boolean,
    baseDirectory: Option[File],
    maxCacheSize:  Long
) {
  require(maxCacheSize >= 0, s"maxCacheSize must be >= 0, but was $maxCacheSize")
}
object CacheSettings {
  val DefaultPositiveTtl = 3600 * 24 * 365
  val DefaultNegativeTtl = 7200

  val Default = CacheSettings(DefaultPositiveTtl, DefaultNegativeTtl, _ => true, None, Long.MaxValue)
}

class DownloadUtils(config: DataServerConfig)(implicit system: ActorSystem) {
  import system.dispatcher
  import CacheSettings.{ DefaultPositiveTtl, DefaultNegativeTtl }

  def computeCache[T](filePattern: T => File, settings: CacheSettings = CacheSettings.Default)(compute: T => Future[File]): T => Future[File] = {
    val lfu = LfuCacheSettings(system).withTimeToLive(settings.ttlSeconds.seconds)
    val s = CachingSettings(system).withLfuCacheSettings(lfu)
    val cache = LfuCache[T, File](s)

    lazy val self: T => Future[File] =
      t => cache.getOrLoad(t, _ => cached(filePattern(t), settings.ttlSeconds, settings.negTtlSeconds, settings.isValid) { () =>
        if (settings.maxCacheSize < Long.MaxValue)
          settings.baseDirectory.foreach { dir =>
            val files = deepFileList(dir).toVector.sortBy(_.lastModified())
            val size = files.map(_.length()).sum
            if (size > 0.9f * settings.maxCacheSize) {
              val deleteTarget = settings.maxCacheSize * 3 / 4
              val toDeleteBytes = size - deleteTarget
              println(s"Cache size of $dir with $size exceeds 90% of configured size ${settings.maxCacheSize}, deleting old files. Pruning to 75%, i.e. $deleteTarget, need to delete at least $toDeleteBytes")
              val toDelete = files.scanLeft(0L)(_ + _.length()).indexWhere(_ > toDeleteBytes)
              if (toDelete > 0) {
                println(s"Deleting ${toDelete} files")
                files.take(toDelete).foreach(_.delete())
              }
            }
          }
        compute(t)
      })
        .flatMap { f =>
          if (!f.exists()) {
            println(s"Cached file missing, removing from cache, and rerunning for ${t}")
            cache.remove(t)
            self(t)
          } else Future.successful(f)
        }
        .map { f =>
          f.setLastModified(System.currentTimeMillis())
          f
        }
    self
  }
  def deepFileList(dir: File): Iterator[File] =
    dir.listFiles().iterator.flatMap {
      case f if f.isDirectory && f.getName != ".." && f.getName != "." => deepFileList(f)
      case f => Iterator(f)
    }

  def downloadCache[T](
    urlPattern:            T => String,
    filePattern:           T => File,
    settings:              CacheSettings = CacheSettings.Default,
    maxConcurrentRequests: Int           = 16): T => Future[File] = {
    val limited = semaphore[T, File](maxConcurrentRequests)(t => download(urlPattern(t), filePattern(t)))
    computeCache(filePattern, settings)(limited)
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
