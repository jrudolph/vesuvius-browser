package net.virtualvoid.vesuvius

import java.io.{ File, RandomAccessFile }
import java.nio.ByteOrder
import java.nio.channels.FileChannel.MapMode
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

  lazy val map = new Iterable[((Int, Int), (Int, Int, Int))] {
    def iterator: Iterator[((Int, Int), (Int, Int, Int))] =
      for {
        u <- (0 until width).iterator
        v <- 0 until height
        p = at(u, v) if p._1 > 0 || p._2 > 0 || p._3 > 0
      } yield (u, v) -> p
  }

  val uvs =
    for {
      u <- (0 until width)
      v <- 0 until height
      uv = UV(u, v)
      if uv.isValid
    } yield uv

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
  val filtered = map.filter {
    case (_, (x, y, z)) =>
      x / 500 == 6 && y / 500 == 10 && z / 500 == 23
  }
  span("x", filtered.map(_._2._1).toVector)
  span("y", filtered.map(_._2._2).toVector)
  span("z", filtered.map(_._2._3).toVector)
  println(filtered.size)
  filtered.groupBy {
    case (_, (x, y, z)) => z
  }.mapValues(_.size).toVector.sortBy(-_._2).take(30).foreach(println)
}
