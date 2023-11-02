package net.virtualvoid.vesuvius

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object PPMReaderTest {
  def main(args: Array[String]): Unit =
    new PPMReaderTest2(args)
}

class PPMReaderTest2(args: Array[String]) {
  //20231011111857
  //20230520175435
  //20230504093154
  //20230827161847
  val segment = "20230926164853"
  val file = new File(Option(args).getOrElse(Array.empty[String]).lift(0).getOrElse(s"/tmp/$segment.ppm"))
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
      val h = vals.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(_._2).reverse
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
    uvs.filter(_.z % 500 == 0).groupBy(_.z).view.mapValues { uvs =>
      val minX = uvs.iterator.map(_.x).min
      val maxX = uvs.iterator.map(_.x).max
      val minY = uvs.iterator.map(_.y).min
      val maxY = uvs.iterator.map(_.y).max
      val centerX = (minX + maxX) / 2
      val centerY = (minY + maxY) / 2
      centerX -> centerY
    }.toMap

  centerByZ.toVector.sortBy(_._1).foreach(println)

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

    val rSpanByZ = uvs.groupBy(_.z).view.mapValues { uvs =>
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

    //val minX = uvs.iterator.map(_.x).min
    //val maxX = uvs.iterator.map(_.x).max
    //val xSpan = maxX - minX

    //val minY = uvs.iterator.map(_.y).min
    //val maxY = uvs.iterator.map(_.y).max
    //val ySpan = maxY - minY

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

/*
Centers as determined with 20230702185753

z,center_x,center_y
1000,3943,2363
1500,3892,2285
2000,3860,2255
2500,3826,2227
3000,3804,2212
3500,3789,2238
4000,3738,2227
4500,3766,2393
5000,3778,2587
5500,3754,2816
6000,3782,3097
6500,3742,3341
7000,3718,3455
7500,3631,3470
8000,3584,3414
8500,3513,3426
9000,3379,3511
9500,3283,3538
10000,3273,3589
10500,3199,3726
11000,3132,3850
11500,3060,4006
12000,2985,4135
12500,2942,4309
13000,2881,4466
13500,2854,4668

*/