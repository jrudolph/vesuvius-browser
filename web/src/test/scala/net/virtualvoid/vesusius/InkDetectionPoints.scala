package net.virtualvoid.vesusius

import net.virtualvoid.vesuvius.{ PPMReader, UV }

import java.io.{ BufferedOutputStream, File }
import javax.imageio.ImageIO

case class V3(x: Int, y: Int, z: Int)
case class BBox(topLeft: V3, bottomRight: V3)

sealed trait Octree[T] {
  def topLeft: V3
  def width: Int
  def depth: Int

  def insert(element: (V3, T)): Octree[T]
  def get(v3: V3): Option[T]
  def size: Int
}
case class Leaf[T](topLeft: V3, width: Int, depth: Int, elements: Seq[(V3, T)]) extends Octree[T] {
  def insert(element: (V3, T)): Octree[T] = {
    require(element._1.x >= topLeft.x && element._1.x < topLeft.x + width)
    require(element._1.y >= topLeft.y && element._1.y < topLeft.y + width)
    require(element._1.z >= topLeft.z && element._1.z < topLeft.z + width)

    if (elements.size < 100 || width == 1) copy(elements = elements :+ element)
    else {
      val newWidth = width / 2
      val newDepth = depth + 1

      val leafs =
        for {
          z <- 0 until 2
          y <- 0 until 2
          x <- 0 until 2
        } yield {
          val newTopLeft = V3(topLeft.x + x * newWidth, topLeft.y + y * newWidth, topLeft.z + z * newWidth)
          Leaf[T](newTopLeft, newWidth, newDepth, Seq.empty)
        }
      val branch = Branch(topLeft, width, depth, 0, leafs)
      elements.foldLeft(branch) { case (tree, element) => tree.insert(element) }
    }
  }

  def get(v3: V3): Option[T] = elements.find(_._1 == v3).map(_._2)

  def size: Int = elements.size
}
case class Branch[T](topLeft: V3, width: Int, depth: Int, size: Int, children: Seq[Octree[T]]) extends Octree[T] {
  def insert(element: (V3, T)): Branch[T] =
    mapChild(octandFor(element._1), _.insert(element)).copy(size = size + 1)

  def get(v3: V3): Option[T] = children(octandFor(v3)).get(v3)

  private def octandFor(coord: V3): Int = {
    val xBit = (coord.x - topLeft.x) * 2 / width
    val yBit = (coord.y - topLeft.y) * 2 / width
    val zBit = (coord.z - topLeft.z) * 2 / width

    xBit + yBit * 2 + zBit * 4
  }

  def mapChild(child: Int, f: Octree[T] => Octree[T]): Branch[T] = {
    val newChildren = children.updated(child, f(children(child)))
    copy(children = newChildren)
  }
}

object InkDetectionPoints extends App {
  val ppm = new File("/home/johannes/tmp/pap/20230827161847.ppm")
  implicit val reader: PPMReader = PPMReader(ppm)

  val inkDetection = new File("../data/inferred/scroll1/20230827161847/inference_youssef-test_15_32.png")
  val img = ImageIO.read(inkDetection)
  val raster = img.getRaster.getDataBuffer
  //val size = raster.getSize
  val threshold = 128

  /*val minx = 0
  val maxx = img.getWidth
  val miny = 0
  val maxy = img.getHeight*/

  val minx = 2494
  val maxx = 3561
  val miny = 700
  val maxy = 4700

  val xyzs =
    (for {
      v <- (miny until maxy).iterator
      u <- (minx until maxx).iterator
      value = raster.getElem(u + v * img.getWidth)
      if value > threshold
      ppm = UV(u, v)
      w <- (-1 to 5).iterator
    } yield {
      val (x, y, z) = ppm.dxyz
      val (nx, ny, nz) = ppm.dnxyz
      ((x + w * nx).toInt, (y + w * ny).toInt, (z + w * nz).toInt)
    }
    ).distinct

  //val min_x = xyzs.minBy(_._1)._1
  //val min_y = xyzs.minBy(_._2)._2
  //val min_z = xyzs.minBy(_._3)._3
  //val max_x = xyzs.maxBy(_._1)._1
  //val max_y = xyzs.maxBy(_._2)._2
  //val max_z = xyzs.maxBy(_._3)._3

  println("Ink detection points")
  //println(s"xyzs: ${xyzs.size}")
  //println(f"min: $min_x $min_y $min_z")
  //println(f"max: $max_x $max_y $max_z")

  //val tiles = xyzs.groupBy { case (x, y, z) => (x / 64, y / 64, z / 64) }.size
  //println(s"Tiles: $tiles")

  val outFile = new File("20230827161847-4.blocks")
  val out = new BufferedOutputStream(new java.io.FileOutputStream(outFile))
  def u16(i: Int): Unit = {
    out.write((i & 0xff).toByte)
    out.write(((i >> 8) & 0xff).toByte)
  }
  xyzs.foreach {
    case (x, y, z) =>
      u16(x)
      u16(y)
      u16(z)
  }
  out.close()
  println(s"written size: ${outFile.length()}")

  val tree = Leaf[Unit](V3(0, 0, 0), width = 16384, 1, Seq.empty)
  val tree1 = xyzs.foldLeft(tree: Octree[Unit])((tree, xyz) => tree.insert((V3(xyz._1, xyz._2, xyz._3), ())))

  def printTree[T](tree: Octree[T], indent: String = ""): Unit = tree match {
    case Leaf(topLeft, width, depth, elements) =>
      println(indent + f"Leaf($topLeft, $width, $depth, ${elements.size})")
    case Branch(topLeft, width, depth, size, children) =>
      println(indent + f"Branch($topLeft, $width, $depth, $size)")
      children.foreach(printTree(_, indent + "  "))
  }

  //printTree(tree1)

  //println(f"Num over threshold: $numOverThreshold percent: ${numOverThreshold.toDouble / size * 100}%5.2f %%")
}
