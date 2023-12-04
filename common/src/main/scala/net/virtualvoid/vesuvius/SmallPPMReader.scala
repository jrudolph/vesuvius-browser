package net.virtualvoid.vesuvius

import java.io.{ File, RandomAccessFile }

trait SmallPPMReader {
  def xyz(u: Int, v: Int): (Int, Int, Int)
}
object SmallPPMReader {
  def apply(file: File, input: DownsamplePPMWorkItemInput, width: Int, height: Int): SmallPPMReader = {
    require(input.positionType == "u16")

    val raf = new RandomAccessFile(file, "r")
    val map = raf.getChannel.map(java.nio.channels.FileChannel.MapMode.READ_ONLY, 0, raf.length)
    val shorts = map.order(java.nio.ByteOrder.LITTLE_ENDIAN).asShortBuffer()

    new SmallPPMReader {
      def xyz(u: Int, v: Int): (Int, Int, Int) =
        if (u < 0 || u >= width || v < 0 || v >= height) (0, 0, 0)
        else {
          val pos = ((v >> input.downsamplingBits) * (width >> input.downsamplingBits) + (u >> input.downsamplingBits)) * 3
          if (pos + 2 >= shorts.limit()) (0, 0, 0)
          else {
            val x = shorts.get(pos)
            val y = shorts.get(pos + 1)
            val z = shorts.get(pos + 2)
            (x.toInt, y.toInt, z.toInt)
          }
        }
    }
  }
}