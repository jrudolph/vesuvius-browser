package net.virtualvoid.vesuvius

import java.awt.image.BufferedImage
import java.io.{ File, RandomAccessFile }
import java.nio.ByteOrder
import java.nio.channels.FileChannel.MapMode
import javax.imageio.ImageIO
import scala.io.Source

object PPMReader extends App {
  val file = new File("/tmp/20230827161847.ppm")
  val source = Source.fromFile(file)
  val lines = source.getLines

  val meta = lines.takeWhile(_ != "<>").toVector
  val fields = meta.map(_.split(": ")).map {
    case Array(key, value) ⇒ key → value
  }.toMap
  fields.foreach(println)
  val offset = meta.map(_.length + 1).sum + 3 /* "<>\n" */
  println(offset)

  val width = fields("width").toInt
  val height = fields("height").toInt

  val raf = new RandomAccessFile(file, "r")

  val chunkHeight = ((1L << 30) / width / 48).toInt
  val vChunks = (height + chunkHeight - 1) / chunkHeight

  val chunks = (0 until vChunks).map { vChunk =>
    val off = offset + vChunk * width * 48 * chunkHeight
    val size = math.min(width * 48 * chunkHeight, file.length() - off)
    println(s"Mapping chunk $vChunk at $off with size $size chunkHeight=$chunkHeight yChunks=$vChunks")
    val map = raf.getChannel.map(MapMode.READ_ONLY, off, size)
    map.order(ByteOrder.LITTLE_ENDIAN)
    map.asDoubleBuffer()
  }
  //val mapped = raf.getChannel.map(MapMode.READ_ONLY, offset, (file.length() - offset) / 2)
  //mapped.order(ByteOrder.LITTLE_ENDIAN)
  //val data = mapped.asDoubleBuffer()

  def at(u: Int, v: Int): (Int, Int, Int) =
    (
      chunks(v / chunkHeight).get(((v % chunkHeight) * width + u) * 6).toInt,
      chunks(v / chunkHeight).get(((v % chunkHeight) * width + u) * 6 + 1).toInt,
      chunks(v / chunkHeight).get(((v % chunkHeight) * width + u) * 6 + 2).toInt
    )

  class UV(val c: Long) extends AnyVal {
    def u: Int = ((c >> 32) & 0xfffff).toInt
    def v: Int = (c & 0xfffff).toInt

    def xyz: (Int, Int, Int) = at(u, v)
    def x: Int = chunks(v / chunkHeight).get(((v % chunkHeight) * width + u) * 6).toInt
    def y: Int = chunks(v / chunkHeight).get(((v % chunkHeight) * width + u) * 6 + 1).toInt
    def z: Int = chunks(v / chunkHeight).get(((v % chunkHeight) * width + u) * 6 + 2).toInt

    def isValid: Boolean = x > 0 || y > 0 || z > 0

    override def toString: String =
      f"(u=$u%5d,v=$v%5d) -> (x=$x%5d,y=$y%5d,z=$z%5d)"
  }
  object UV {
    def apply(u: Int, v: Int): UV = new UV((u.toLong << 32) | v)
  }

  lazy val uvs = new Iterable[UV] {
    def iterator: Iterator[UV] =
      for {
        u <- (0 until width).iterator
        v <- (0 until height).iterator
        uv = UV(u, v)
        if uv.isValid
      } yield uv
  }

  def span(what: String, s: Seq[Int]): Unit = {
    val min = s.min
    val max = s.max
    println(s"$what: $min - $max (${max - min})")
  }

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

  def uvmapvis(): Unit = {
    val minX = uvs.iterator.map(_.x).min
    val maxX = uvs.iterator.map(_.x).max
    val xSpan = maxX - minX

    val minY = uvs.iterator.map(_.y).min
    val maxY = uvs.iterator.map(_.y).max
    val ySpan = maxY - minY

    val centerX = (minX + maxX) / 2
    val centerY = (minY + maxY) / 2

    val rSpan = math.sqrt((maxX - centerX) * (maxX - centerX) + (maxY - centerY) * (maxY - centerY))

    val minZ = (uvs.iterator.map(_.z).min - 500) max 0
    val maxZ = uvs.iterator.map(_.z).max
    val zSpan = maxZ - minZ

    println(f"X: $minX%10d - $maxX%10d ($xSpan%10d)")
    println(f"Y: $minY%10d - $maxY%10d ($ySpan%10d)")
    println(f"Z: $minZ%10d - $maxZ%10d ($zSpan%10d)")
    println(f"Center: $centerX%10d / $centerY%10d")
    println(f"R: $rSpan%7.4f")

    val xMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val yMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val zMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val thetaMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val rMap = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    uvs.foreach { uv =>
      xMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0.5, 1, (uv.x.toFloat - minX) / xSpan))
      yMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0.3, 1, (uv.y.toFloat - minY) / ySpan))
      zMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0, 1, (uv.z.toFloat - minZ) / zSpan))

      val theta = math.atan2(uv.x - centerX, uv.y - centerY)
      val r = math.hypot(uv.x - centerX, uv.y - centerY)

      thetaMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(((theta + math.Pi) / (2 * math.Pi)).toFloat, 1, 1))
      rMap.setRGB(uv.u, uv.v, java.awt.Color.HSBtoRGB(0.8, 1, (r / rSpan).toFloat))
    }
    ImageIO.write(xMap, "png", new File("/tmp/xmap.png"))
    ImageIO.write(yMap, "png", new File("/tmp/ymap.png"))
    ImageIO.write(zMap, "png", new File("/tmp/zmap.png"))
    ImageIO.write(thetaMap, "png", new File("/tmp/thetamap.png"))
    ImageIO.write(rMap, "png", new File("/tmp/rmap.png"))
  }
}
