package net.virtualvoid.vesuvius

import com.typesafe.config.Config
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.caching.scaladsl.{ CachingSettings, LfuCacheSettings }
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ HttpMethods, HttpRequest, StatusCodes, headers }
import org.apache.pekko.stream.scaladsl.{ FileIO, Keep, Sink }

import java.io.File
import scala.concurrent.duration.*
import scala.concurrent.{ Future, Promise, TimeoutException }
import scala.util.{ Failure, Success, Try }

trait DataServerConfig {
  def dataServerUsername: String
  def dataServerPassword: String
}
object DataServerConfig {
  def fromConfig(config: Config): DataServerConfig =
    Impl(
      dataServerUsername = config.getString("app.data-username"),
      dataServerPassword = config.getString("app.data-password")
    )
  private case class Impl(dataServerUsername: String, dataServerPassword: String) extends DataServerConfig
}

case class CacheSettings(
    ttlSeconds:         Long,
    negTtlSeconds:      Long,
    isValid:            File => Boolean,
    baseDirectory:      Option[File],
    maxCacheSize:       Long,
    cacheHighWatermark: Double,
    cacheLowWatermark:  Double
) {
  require(maxCacheSize >= 0, s"maxCacheSize must be >= 0, but was $maxCacheSize")
}
object CacheSettings {
  val DefaultPositiveTtl = 3600 * 24 * 365
  val DefaultNegativeTtl = 7200

  val Default = CacheSettings(DefaultPositiveTtl, DefaultNegativeTtl, _ => true, None, Long.MaxValue, 0.9, 0.75)
}

trait Cache[T, U] {
  def apply(t: T): Future[U]
  def contains(t: T): Boolean
  def isReady(t: T): Boolean
  def remove(t: T): Unit
}

class DownloadUtils(config: DataServerConfig)(implicit system: ActorSystem) {
  import CacheSettings.{ DefaultNegativeTtl, DefaultPositiveTtl }
  import system.dispatcher

  def computeCache[T](filePattern: T => File, settings: CacheSettings = CacheSettings.Default)(compute: T => Future[File]): Cache[T, File] = {
    val lfu = LfuCacheSettings(system).withTimeToLive(settings.ttlSeconds.seconds)
    val s = CachingSettings(system).withLfuCacheSettings(lfu)
    val fCache = fileCache(filePattern, settings.ttlSeconds, settings.negTtlSeconds, settings.isValid)(compute)
    val cache = LfuCache[T, File](s)

    lazy val self: Cache[T, File] = new Cache[T, File] {
      def apply(t: T): Future[File] = {
        cache.getOrLoad(t, _ => fCache(t))
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
      }

      def contains(t: T): Boolean = cache.get(t).isDefined || fCache.contains(t)
      def isReady(t: T): Boolean = cache.get(t).exists(_.isCompleted) || fCache.isReady(t)
      def remove(t: T): Unit = cache.remove(t)
    }
    self
  }
  def deepFileList(dir: File): Iterator[File] =
    dir.listFiles().iterator.flatMap {
      case f if f.isDirectory && f.getName != ".." && f.getName != "." => deepFileList(f)
      case f => Iterator(f)
    }

  class ExpelledException(msg: String) extends TimeoutException(msg)

  def downloadCache[T](
    urlPattern:            T => String,
    filePattern:           T => File,
    settings:              CacheSettings = CacheSettings.Default,
    maxConcurrentRequests: Int           = 16): Cache[T, File] = {
    lazy val (limited: (T => Future[File]), refresh: (T => Unit)) = semaphore[T, File](maxConcurrentRequests) { (t, time) =>
      if (System.nanoTime() - time > 1000000000L * 60) {
        cache.remove(t)
        throw new ExpelledException(s"Request was not refreshed in queue for ${(System.nanoTime() - time) / 1000000000L} seconds, aborting")
      } else {
        cleanupCacheDir(settings)
        download(urlPattern(t), filePattern(t))
      }
    }

    lazy val cache = computeCache(filePattern, settings)(limited)

    new Cache[T, File] {
      def apply(t: T): Future[File] = {
        if (!cache.isReady(t))
          refresh(t)

        cache(t)
      }
      def contains(t: T): Boolean = cache.contains(t)
      def isReady(t: T): Boolean = cache.isReady(t)
      def remove(t: T): Unit = cache.remove(t)
    }
  }

  def cacheDownload(url: String, to: File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl): Future[File] =
    cached(to, ttlSeconds, negTtlSeconds) { () => download(url, to) }

  def fileCache[T](filePattern: T => File, ttlSeconds: Long = DefaultPositiveTtl, negTtlSeconds: Long = DefaultNegativeTtl, isValid: File => Boolean = _ => true)(f: T => Future[File]): Cache[T, File] =
    new Cache[T, File] {
      def apply(t: T): Future[File] = {
        val to = fileFor(t)
        to.getParentFile.mkdirs()
        val neg = new File(to.getParentFile, s".neg-${to.getName}")
        if (to.exists() && to.lastModified() + ttlSeconds * 1000 > System.currentTimeMillis() && isValid(to)) Future.successful(to)
        else if (neg.exists() && neg.lastModified() + negTtlSeconds * 1000 > System.currentTimeMillis() && isValid(neg)) Future.failed(new NoSuchElementException(s"Negatively cached"))
        else
          f(t).recoverWith {
            case t: ExpelledException => Future.failed(t) // do not cache these negative instances
            case t: Throwable =>
              neg.getParentFile.mkdirs()
              neg.delete()
              neg.createNewFile()
              Future.failed(t)
          }
      }

      def contains(t: T): Boolean = fileFor(t).exists()
      def isReady(t: T): Boolean = contains(t)
      def remove(t: T): Unit = () // FIXME: should that be a no-op?

      def fileFor(t: T): File = filePattern(t)
    }

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

  def semaphore[T, U](numRequests: Int, queueLength: Int = 1000)(f: (T, Long) => Future[U]): (T => Future[U], T => Unit) = {
    val (queue, res) =
      PriorityQueue.queue[(T, Promise[U])]()
        .mapAsyncUnordered(numRequests) {
          case ((t, promise), time) =>
            Try(f(t, time)) match {
              case Success(fut) => fut.onComplete(promise.complete)
              case Failure(t)   => promise.failure(t)
            }
            promise.future.transform(Success(_))
        }
        .toMat(Sink.ignore)(Keep.both)
        .run()

    res.onComplete {
      case Success(_) => println("Semaphore stopped")
      case Failure(t) =>
        println(s"Semaphore stopped with failure: $t")
        t.printStackTrace()
    }

    (t => {
      val promise = Promise[U]()
      queue.offer(t -> promise)
      promise.future
    }, t => queue.refresh(_._1 == t))
  }

  def cleanupCacheDir(settings: CacheSettings): Unit = {
    if (settings.maxCacheSize < Long.MaxValue)
      settings.baseDirectory.foreach { dir =>
        val files = deepFileList(dir).map(x => x -> x.lastModified()).toVector.sortBy(_._2).map(_._1)
        val size = files.map(_.length()).sum
        if (size > settings.cacheHighWatermark * settings.maxCacheSize) {
          val deleteTarget = (settings.cacheLowWatermark * settings.maxCacheSize).toLong
          val toDeleteBytes = size - deleteTarget
          println(s"Cache size of $dir with $size exceeds 90% of configured size ${settings.maxCacheSize}, deleting old files. Pruning to 75%, i.e. $deleteTarget, need to delete at least $toDeleteBytes")
          val toDelete = files.scanLeft(0L)(_ + _.length()).indexWhere(_ > toDeleteBytes)
          if (toDelete > 0) {
            println(s"Deleting ${toDelete} files")
            files.take(toDelete).foreach(_.delete())
          }
        }
      }
  }
}
