package net.virtualvoid.vesuvius

import java.io.{ File, RandomAccessFile }
import java.nio.channels.FileChannel.MapMode
import java.nio.{ ByteOrder, DoubleBuffer }
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
      case Array(key, value) => key -> value
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
  def dxyz(implicit p: PPMReader): (Double, Double, Double) = (dvalue(0), dvalue(1), dvalue(2))
  def dnxyz(implicit p: PPMReader): (Double, Double, Double) = (dvalue(3), dvalue(4), dvalue(5))

  def x(implicit p: PPMReader): Int = value(0)
  def y(implicit p: PPMReader): Int = value(1)
  def z(implicit p: PPMReader): Int = value(2)

  def nx(implicit p: PPMReader): Int = value(3)
  def ny(implicit p: PPMReader): Int = value(4)
  def nz(implicit p: PPMReader): Int = value(5)

  private def basePos(implicit p: PPMReader): Int = ((v % p.chunkHeight) * p.width + u) * 6
  private def value(offset: Int)(implicit p: PPMReader): Int = dvalue(offset).toInt
  private def dvalue(offset: Int)(implicit p: PPMReader): Double = p.bufferFor(v).get(basePos + offset)

  def isValid(implicit p: PPMReader): Boolean = x > 0 || y > 0 || z > 0

  override def toString: String = f"UV(u=$u%5d,v=$v%5d)"
}

object UV {
  def apply(u: Int, v: Int): UV = new UV((u.toLong << 32) | v)
}

