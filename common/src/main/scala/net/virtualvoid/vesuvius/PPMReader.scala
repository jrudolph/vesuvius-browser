package net.virtualvoid.vesuvius


import java.awt.image.BufferedImage
import java.io.{File, RandomAccessFile}
import java.nio.channels.FileChannel.MapMode
import java.nio.{ByteOrder, DoubleBuffer}
import javax.imageio.ImageIO
import scala.io.Source

trait PPMReader {
  def width: Int
  def height: Int

  def bufferFor(v: Int): DoubleBuffer
  def chunkHeight: Int

  def uvs: Iterable[UV]
  def allUvs: Iterable[UV]
}
object PPMReader {
  def apply(file: File): PPMReader = {
    val source = Source.fromFile(file)
    val lines = source.getLines

    val meta = lines.takeWhile(_ != "<>").toVector
    val fields = meta.map(_.split(": ")).map {
      case Array(key, value) ⇒ key → value
    }.toMap
    fields.foreach(println)
    val offset = meta.map(_.length + 1).sum + 3 /* "<>\n" */
    println(offset)
    val _width = fields("width").toInt
    val _height = fields("height").toInt

    val raf = new RandomAccessFile(file, "r")

    val _chunkHeight = ((1L << 30) / _width / 48).toInt
    val vChunks = (_height + _chunkHeight - 1) / _chunkHeight

    val chunks = (0 until vChunks).map { vChunk =>
      val off = offset + vChunk.toLong * _width * 48 * _chunkHeight
      val size = math.min(_width * 48 * _chunkHeight, file.length() - off)
      println(s"Mapping chunk $vChunk at $off with size $size chunkHeight=$_chunkHeight yChunks=$vChunks")
      val map = raf.getChannel.map(MapMode.READ_ONLY, off, size)
      map.order(ByteOrder.LITTLE_ENDIAN)
      map.asDoubleBuffer()
    }

    new PPMReader { reader =>
       def width: Int = _width
       def height: Int = _height
       def bufferFor(v: Int): DoubleBuffer = chunks(v / chunkHeight)
       def chunkHeight: Int = _chunkHeight

      lazy val uvs = new Iterable[UV] {
        def iterator: Iterator[UV] =
          for {
            v <- (0 until height).iterator
            u <- (0 until width).iterator
            uv = UV(u, v)
            if uv.isValid(reader)
          } yield uv
      }
      lazy val allUvs = new Iterable[UV] {
        def iterator: Iterator[UV] =
          for {
            v <- (0 until height).iterator
            u <- (0 until width).iterator
            uv = UV(u, v)
          } yield uv
      }
    }
  }
}

class UV(val c: Long) extends AnyVal {
  def u: Int = ((c >> 32) & 0xfffff).toInt

  def v: Int = (c & 0xfffff).toInt

  def xyz(implicit p: PPMReader): (Int, Int, Int) = (x, y, z)

  def x(implicit p: PPMReader): Int = value(0)
  def y(implicit p: PPMReader): Int = value(1)
  def z(implicit p: PPMReader): Int = value(2)

  private def basePos(implicit p: PPMReader): Int = ((v % p.chunkHeight) * p.width + u) * 6
  private def value(offset: Int)(implicit p: PPMReader): Int = p.bufferFor(v).get(basePos + offset).toInt

  def isValid(implicit p: PPMReader): Boolean = x > 0 || y > 0 || z > 0

  override def toString: String = f"UV(u=$u%5d,v=$v%5d)"
}

object UV {
  def apply(u: Int, v: Int): UV = new UV((u.toLong << 32) | v)
}

object PPMReaderTest extends App {
  //20231011111857
  //20230520175435
  //20230504093154
  //20230827161847
  val segment = "20230926164853"
  val file = new File(s"/tmp/$segment.ppm")
  implicit val reader: PPMReader = PPMReader(file)

  val width = reader.width
  val height = reader.height

  import reader.{allUvs, uvs}

  def compressionExperiments(): Unit = {
    println("Scanning")
    val (dxs, dys, dzs) =
      allUvs.scanLeft((0, 0, 0) -> (0, 0, 0)) { (l, n) =>
        val ((lx, ly, lz), _) = l
        (n.xyz, (n.x - lx, n.y - ly, n.z - lz))
      }.drop(1).map(_._2).take(10_000_000).unzip3

    def histo(what: String, vals: Iterable[Int]): Unit = {
      println(s"$what")
      val h = vals.groupBy(identity).mapValues(_.size).toSeq.sortBy(_._2).reverse
      h.take(10).foreach {
        case (v, c) => println(f"$v%5d: $c%6d ${c / vals.size.toDouble * 100}%5.2f%%")
      }
      val cost = h.map {
        case (d, n) =>
          val c = d match {
            case 0 => 1 * n
            case 1 => 3 * n
            case -1 => 2 * n
            case _ => 19 * n
          }
          if (n > 100)
            println(f"$d%5d: $n%6d $c")
          c
      }.sum
      println(f"Cost: $cost bits ${cost / 8}%d bytes")
    }

    histo("dxs", dxs)
    histo("dys", dys)
    histo("dzs", dzs)
  }

  def rotAngleExample(): Unit = {
    def findRotationAngle(uv: UV, r: Int, n: Int): Double = {
      // for all points on the circle with radius r around (u,v)
      val diffs =
        (0 until n)
          .map { idx =>
            val angle = idx * 2 * math.Pi / n
            val u1 = uv.u + (r * math.cos(angle)).toInt
            val v1 = uv.v + (r * math.sin(angle)).toInt
            angle -> UV(u1, v1)
          }
          .filter(_._2.isValid)
          .map {
            case (angle, uv1) =>
              angle -> (uv1.z - uv.z)
          }
      diffs.minBy(_._2)._1
    }

    val samples = 100
    val angles = 500
    val r = 500
    val results =
      (0 until samples).flatMap { _ =>
        val uv = UV(util.Random.nextInt(width - 2 * r) + r, util.Random.nextInt(height - 2 * r) + r)
        if (uv.isValid) {
          val angle = findRotationAngle(uv, r, angles)
          println(f"At $uv Angle: ${angle / 2 / math.Pi * 360}%7.4f")
          Some(angle)
        } else
          None
      }
    val dirSum =
      results
        .map(a => math.sin(a) -> math.cos(a))
        .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    val avgAngle = math.atan2(dirSum._1, dirSum._2)
    println(f"Avg angle: ${avgAngle / 2 / math.Pi * 360}%7.4f")
  }
  rotAngleExample()

  lazy val centerByZ =
    uvs.groupBy(_.z).mapValues { uvs =>
      val minX = uvs.iterator.map(_.x).min
      val maxX = uvs.iterator.map(_.x).max
      val minY = uvs.iterator.map(_.y).min
      val maxY = uvs.iterator.map(_.y).max
      val centerX = (minX + maxX) / 2
      val centerY = (minY + maxY) / 2
      centerX -> centerY
    }.toMap

  def uvmapvis(): Unit = {
    val minX = uvs.iterator.map(_.x).min
    val maxX = uvs.iterator.map(_.x).max
    val xSpan = maxX - minX

    val minY = uvs.iterator.map(_.y).min
    val maxY = uvs.iterator.map(_.y).max
    val ySpan = maxY - minY

    //val centerX = (minX + maxX) / 2
    //val centerY = (minY + maxY) / 2
    //val centerX = 3240 //(minX + maxX) / 2
    //val centerY = 3700 // (minY + maxY) / 2

    val rSpanByZ = uvs.groupBy(_.z).mapValues { uvs =>
      val centerX = centerByZ(uvs.head.z)._1
      val centerY = centerByZ(uvs.head.z)._2
      val rSpan = uvs.iterator.map(uv => math.hypot(uv.x - centerX, uv.y - centerY)).max
      rSpan
    }.toMap

    val minZ = (uvs.iterator.map(_.z).min - 500) max 0
    val maxZ = uvs.iterator.map(_.z).max
    val zSpan = maxZ - minZ

    println(f"X: $minX%10d - $maxX%10d ($xSpan%10d)")
    println(f"Y: $minY%10d - $maxY%10d ($ySpan%10d)")
    println(f"Z: $minZ%10d - $maxZ%10d ($zSpan%10d)")
    //println(f"Center: $centerX%10d / $centerY%10d")
    //println(f"R: $rSpan%7.4f")

    val xMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val yMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val zMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val thetaMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val rMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    uvs.foreach { uv =>
      xMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0.5, 1, (uv.x.toFloat - minX) / xSpan))
      yMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0.3, 1, (uv.y.toFloat - minY) / ySpan))
      zMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0, 1, (uv.z.toFloat - minZ) / zSpan))

      val (centerX, centerY) = centerByZ(uv.z)

      val theta = math.atan2(uv.x - centerX, uv.y - centerY)
      val r = math.hypot(uv.x - centerX, uv.y - centerY)

      thetaMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(((theta + math.Pi) / (2 * math.Pi)).toFloat, 1, 1))
      rMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0.8, 1, (r / rSpanByZ(uv.z)).toFloat))
    }
    ImageIO.write(xMap, "png", new File(s"/tmp/$segment-xmap.png"))
    ImageIO.write(yMap, "png", new File(s"/tmp/$segment-ymap.png"))
    ImageIO.write(zMap, "png", new File(s"/tmp/$segment-zmap.png"))
    ImageIO.write(thetaMap, "png", new File(s"/tmp/$segment-thetamap.png"))
    ImageIO.write(rMap, "png", new File(s"/tmp/$segment-rmap.png"))
  }

  //uvmapvis()
  def findExtents(): Unit = {
    println("Finding extents of segment")

    val minX = uvs.iterator.map(_.x).min
    val maxX = uvs.iterator.map(_.x).max
    val xSpan = maxX - minX

    val minY = uvs.iterator.map(_.y).min
    val maxY = uvs.iterator.map(_.y).max
    val ySpan = maxY - minY

    val minZ = uvs.iterator.map(_.z).min
    val maxZ = uvs.iterator.map(_.z).max
    val middleZ = (minZ + maxZ) / 2

    val (centerX, centerY) = centerByZ(middleZ) //3240 //(minX + maxX) / 2
    //val centerY = //3700 // (minY + maxY) / 2
    println(f"Center: $centerX%10d / $centerY%10d")

    println("Tracing at middle z")
    val entries =
    uvs.flatMap { uv =>
      val theta = math.atan2(uv.x - centerX, uv.y - centerY) + math.Pi
      val r = math.hypot(uv.x - centerX, uv.y - centerY)
      val thetaQ = math.round(theta / 2f / math.Pi * 720)

      if (uv.z == middleZ && thetaQ.toInt % 90 == 0) {
        println(f"At $uv theta=${thetaQ / 2}%7.4f r=$r%7.4f")
        Some(thetaQ / 2 % 360-> r)
      } else
        None
    }

    val minLayerThickness = 10
    def simplify(entries: Seq[Double]): Seq[Double] =
      (entries :+ 100000d).scanLeft((0.0 -> Option.empty[Double])) { (cur, next) =>
        if (math.abs(cur._1 - next) < minLayerThickness) ((cur._1 + next) / 2, None)
        else (next, Some(cur._1))
      }.flatMap(_._2).drop(1)

      /*entries.sliding(2).flatMap {
        case Seq(a, b) =>
          if (math.abs(a - b) < minLayerThickness) None
          else Some(a + b / 2)
      }.toVector*/

    entries.groupBy(_._1)
      .toVector
      .flatMap {
        case (theta, entries) =>
          simplify(entries.map(_._2).toVector.sorted).map(theta -> _)
      }
      .foreach(println)
  }
  findExtents()
}
