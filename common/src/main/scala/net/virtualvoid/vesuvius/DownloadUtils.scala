package net.virtualvoid.vesuvius

import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.caching.LfuCache
import org.apache.pekko.http.caching.scaladsl.{ CachingSettings, LfuCacheSettings }
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{ HttpHeader, HttpMethods, HttpRequest, HttpResponse, StatusCodes, headers }
import org.apache.pekko.stream.scaladsl.{ FileIO, Flow, Keep, Sink }
import org.apache.pekko.util.ByteString

import java.io.{ File, PrintStream }
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.*
import scala.concurrent.{ Future, Promise, TimeoutException }
import scala.util.{ Failure, Success, Try }

trait DataServerConfig {
  def dataServerUsername: String
  def dataServerPassword: String
}

case class CacheSettings(
    ttl:                Duration,
    negativeTtl:        Duration,
    isValid:            File => Boolean,
    baseDirectory:      Option[File],
    maxCacheSize:       Long,
    cacheHighWatermark: Double,
    cacheLowWatermark:  Double
) {
  require(maxCacheSize >= 0, s"maxCacheSize must be >= 0, but was $maxCacheSize")
}
object CacheSettings {
  val DefaultPositiveTtl = 365.days
  val DefaultNegativeTtl = 2.hours

  val Default = CacheSettings(DefaultPositiveTtl, DefaultNegativeTtl, _ => true, None, Long.MaxValue, 0.9, 0.75)
}

trait Cache[T, U] {
  def apply(t: T): Future[U]
  def contains(t: T): Boolean
  def isReady(t: T): Boolean
  def remove(t: T): Unit

  def map[V](f: (T, U) => V)(implicit system: ActorSystem): Cache[T, V] = new Cache[T, V] {
    val settings = CacheSettings.Default
    val lfu = LfuCacheSettings(system).withTimeToLive(settings.ttl)
    val s = CachingSettings(system).withLfuCacheSettings(lfu)
    val cache = LfuCache[T, V](s)

    def apply(t: T): Future[V] = cache.getOrLoad(t, t => Cache.this(t).map(u => f(t, u))(system.dispatcher))
    def contains(t: T): Boolean = cache.get(t).isDefined || Cache.this.contains(t)
    def isReady(t: T): Boolean = cache.get(t).exists(_.isCompleted) || Cache.this.isReady(t)
    def remove(t: T): Unit = {
      cache.remove(t)
      Cache.this.remove(t)
    }
  }
}

trait FileCache[T] extends Cache[T, File] {
  def targetFile(t: T): File
}

class DownloadUtils(config: DataServerConfig)(implicit system: ActorSystem) {
  import CacheSettings.{ DefaultNegativeTtl, DefaultPositiveTtl }
  import system.dispatcher
  val blockingDispatcher = system.dispatchers.lookup("pekko.actor.default-blocking-io-dispatcher")
  val downloadCounter = DownloadCounter()

  def computeCache[T](filePattern: T => File, settings: CacheSettings = CacheSettings.Default)(compute: T => Future[File]): FileCache[T] = {
    val lfu = LfuCacheSettings(system).withTimeToLive(settings.ttl)
    val s = CachingSettings(system).withLfuCacheSettings(lfu)
    val fCache = fileCache(filePattern, settings.ttl, settings.negativeTtl, settings.isValid)(compute)
    val cache = LfuCache[T, File](s)

    lazy val self: FileCache[T] = new FileCache[T] {
      def apply(t: T): Future[File] = {
        cache.getOrLoad(t, { _ =>
          val res = fCache(t)
          // FIXME: shouldn't access registration go to the fileCache?
          res.foreach { f =>
            f.setLastModified(System.currentTimeMillis())
            f
          }(blockingDispatcher)
          res
        })
          .flatMap { f =>
            if (!f.exists()) {
              println(s"Cached file missing, removing from cache, and rerunning for ${t}")
              cache.remove(t)
              self(t)
            } else Future.successful(f)
          }(blockingDispatcher)
      }

      def contains(t: T): Boolean = cache.get(t).isDefined || fCache.contains(t)
      def isReady(t: T): Boolean = cache.get(t) match {
        case Some(x) => x.isCompleted
        case None    => fCache.isReady(t)
      }
      def remove(t: T): Unit = cache.remove(t)

      def targetFile(t: T): File = filePattern(t)
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
    maxConcurrentRequests: Int           = 16): FileCache[T] =
    requestDownloadCache(t => authenticatedDataServerRequest(urlPattern(t)), filePattern, receiveBody, settings, maxConcurrentRequests)

  def requestDownloadCache[T](
    requestPattern:        T => HttpRequest,
    filePattern:           T => File,
    receiveFile:           (HttpResponse, HttpRequest, File) => Future[Done],
    settings:              CacheSettings                                     = CacheSettings.Default,
    maxConcurrentRequests: Int                                               = 16): FileCache[T] = {
    lazy val (limited: (T => Future[File]), refresh: (T => Unit)) = semaphore[T, File](maxConcurrentRequests) { (t, time) =>
      if (System.nanoTime() - time > 1000000000L * 60) {
        cache.remove(t)
        throw new ExpelledException(s"Request was not refreshed in queue for ${(System.nanoTime() - time) / 1000000000L} seconds, aborting")
      } else {
        //cleanupCacheDir(settings)
        download(requestPattern(t), filePattern(t), receiveFile)
      }
    }

    lazy val cache = computeCache(filePattern, settings)(limited)

    new FileCache[T] {
      def apply(t: T): Future[File] = {
        if (!cache.isReady(t))
          refresh(t)

        cache(t)
      }
      def contains(t: T): Boolean = cache.contains(t)
      def isReady(t: T): Boolean = cache.isReady(t)
      def remove(t: T): Unit = cache.remove(t)

      def targetFile(t: T): File = filePattern(t)
    }
  }

  def cacheDownload(url: String, to: File, ttl: Duration = DefaultPositiveTtl, negativeTtl: Duration = DefaultNegativeTtl): Future[File] =
    cached(to, ttl, negativeTtl) { () => download(url, to) }

  def isDeadlineInTheFuture(originMillis: Long, deadlineDuration: Duration): Boolean =
    !deadlineDuration.isFinite ||
      originMillis + deadlineDuration.toMillis > System.currentTimeMillis()

  def fileCache[T](filePattern: T => File, ttl: Duration = DefaultPositiveTtl, negativeTtl: Duration = DefaultNegativeTtl, isValid: File => Boolean = _ => true)(f: T => Future[File]): Cache[T, File] =
    new Cache[T, File] {
      def apply(t: T): Future[File] = {
        val to = fileFor(t)
        to.getParentFile.mkdirs()
        val neg = new File(to.getParentFile, s".neg-${to.getName}")
        if (to.exists() && isDeadlineInTheFuture(to.lastModified(), ttl) && isValid(to)) Future.successful(to)
        else if (neg.exists() && isDeadlineInTheFuture(neg.lastModified(), negativeTtl) && isValid(neg)) Future.failed(new NoSuchElementException(s"Negatively cached for $t file: $to"))
        else
          f(t).recoverWith {
            case t: ExpelledException => Future.failed(t) // do not cache these negative instances
            case t: Throwable =>
              neg.getParentFile.mkdirs()
              neg.delete()
              neg.createNewFile()
              val fos = new java.io.FileOutputStream(neg)
              t.printStackTrace(new PrintStream(fos))
              fos.close()
              Future.failed(t)
          }
      }

      def contains(t: T): Boolean = fileFor(t).exists()
      def isReady(t: T): Boolean = contains(t)
      def remove(t: T): Unit = () // FIXME: should that be a no-op?

      def fileFor(t: T): File = filePattern(t)
    }

  import spray.json._
  def jsonCache[K, V: JsonFormat](filePattern: K => File, ttl: Duration = DefaultPositiveTtl, negativeTtl: Duration = DefaultNegativeTtl, isValid: File => Boolean = _ => true)(f: K => Future[V]): Cache[K, V] =
    fileCache(filePattern, ttl, negativeTtl, isValid) { k =>
      f(k).map { v =>
        val file = filePattern(k)
        file.getParentFile.mkdirs()
        val out = new java.io.FileOutputStream(file)
        out.write(v.toJson.compactPrint.getBytes("utf8"))
        out.close()
        file
      }
    }.map { (k, file) =>
      val source = scala.io.Source.fromFile(file)
      val json = source.mkString
      source.close()
      json.parseJson.convertTo[V]
    }

  def cached(to: File, ttl: Duration = DefaultPositiveTtl, negativeTtl: Duration = DefaultNegativeTtl, isValid: File => Boolean = _ => true)(f: () => Future[File]): Future[File] = {
    to.getParentFile.mkdirs()
    val neg = new File(to.getParentFile, s".neg-${to.getName}")
    if (to.exists() && isDeadlineInTheFuture(to.lastModified(), ttl) && isValid(to)) Future.successful(to)
    else if (neg.exists() && isDeadlineInTheFuture(neg.lastModified(), negativeTtl) && isValid(neg)) Future.failed(new RuntimeException(s"Negatively cached file: $to"))
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
  def authenticatedDataServerRequest(url: String, additionalHeaders: Seq[HttpHeader] = Nil): HttpRequest =
    HttpRequest(HttpMethods.GET, uri = url, headers = auth +: additionalHeaders)
  def download(url: String, to: File): Future[File] =
    download(authenticatedDataServerRequest(url), to, receiveBody)

  def receiveBody(res: HttpResponse, request: HttpRequest, target: File): Future[Done] = {
    require(res.status == StatusCodes.OK, s"Got status ${res.status} for ${request.uri}")
    res.entity.dataBytes.runWith(FileIO.toPath(target.toPath)).map(_ => Done) // FIXME: handle io errors
  }

  def download(request: HttpRequest, to: File, receiveFile: (HttpResponse, HttpRequest, File) => Future[Done]): Future[File] = {
    println(s"Downloading ${request.uri}")
    val start = System.currentTimeMillis()
    val tmpFile = new File(to.getParentFile, s".tmp.${to.getName}")
    Http().singleRequest(request)
      .map(_.transformEntityDataBytes(Flow[ByteString].map(downloadCounter.countBytes)))
      .flatMap(receiveFile(_, request, tmpFile))
      .map { _ =>
        val end = System.currentTimeMillis()
        println(f"Downloading ${request.uri} length: ${tmpFile.length()} finished after ${end - start} ms (${(tmpFile.length().toFloat / 1000f / (end - start).toFloat)}%6.3f MB/s)")
        tmpFile.renameTo(to)
        to
      }
  }

  def semaphore[T, U](numRequests: Int, queueLength: Int = 1000)(f: (T, Long) => Future[U]): (T => Future[U], T => Unit) = {
    val (queue, res) =
      PriorityQueue.queue[T, Promise[U]]()
        .mapAsyncUnordered(numRequests) {
          case (t, promise, time) =>
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
      queue.offer(t, promise)
      promise.future
    }, (t: T) => queue.refresh(t))
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

  def urlExists(url: String): Future[Boolean] = {
    println(s"Checking existence of $url")
    head(url).map(_.status == StatusCodes.OK)
  }
  def head(url: String): Future[HttpResponse] =
    Http().singleRequest(authenticatedDataServerRequest(url).withMethod(method = HttpMethods.HEAD))
}

trait DownloadCounter {
  def countBytes(bytes: ByteString): ByteString
}
object DownloadCounter {
  def apply(): DownloadCounter =
    new DownloadCounter {
      val totalDownloaded = new AtomicLong()
      val lastDownloadReport = new AtomicLong(System.currentTimeMillis())
      val lastReportDownloaded = new AtomicLong()
      val reportDownloadMillis = 10000

      def countBytes(bytes: ByteString): ByteString = {
        val total = totalDownloaded.addAndGet(bytes.size)
        val last = lastDownloadReport.get()
        val now = System.currentTimeMillis()
        if (last + reportDownloadMillis < now) {
          if (lastDownloadReport.compareAndSet(last, now)) { // we won the race
            val old = lastReportDownloaded.getAndSet(total)
            val downloaded = total - old
            val lastedMillis = now - last
            println(f"10s download avg: ${downloaded.toDouble / 1024d / 1024d}%6.3fMB. Thpt: ${downloaded / 1024d / 1024d * 1000d / lastedMillis}%5.2fMB/s")
          }
        }
        bytes
      }
    }
}
