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

  /*span("x", uvs.map(_.x).toVector)
  span("y", uvs.map(_.y).toVector)
  span("z", uvs.map(_.z).toVector)*/

  /*val groups = map.groupBy {
    case (_, (x, y, z)) =>
      (x / 500, y / 500, z / 500)
  }
  groups.mapValues(_.size).toVector.sortBy(-_._2).foreach(println)
  println(groups.size)*/
  /*val filtered = map.filter {
    case (_, (x, y, z)) =>
      x / 500 == 6 && y / 500 == 10 && z / 500 == 23
  }
  span("x", filtered.map(_._2._1).toVector)
  span("y", filtered.map(_._2._2).toVector)
  span("z", filtered.map(_._2._3).toVector)
  println(filtered.size)
  filtered.groupBy {
    case (_, (x, y, z)) => z
  }.mapValues(_.size).toVector.sortBy(-_._2).take(30).foreach(println)*/

  /*uvs.groupBy(_.u).toVector.sortBy(_._1).take(10).foreach {
    case (u, uvs) =>
      val min = uvs.minBy(_.v)
      val max = uvs.maxBy(_.v)
      println(s"u: $u minV: ${min.v} maxV: ${max.v}")
  }*/

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
