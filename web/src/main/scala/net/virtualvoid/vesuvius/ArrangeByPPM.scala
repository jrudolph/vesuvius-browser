package net.virtualvoid.vesuvius

import java.io.File
import spray.json.*

import java.awt.{ AlphaComposite, Graphics2D }
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.io.Source

class ArrangeByPPM(segmentInfos: Map[SegmentReference, ImageInfo]) {
  val dir = new File("../data/ppm/scroll1")

  // find all json files in subdirs
  val fingerprints =
    dir.listFiles()
      .filter(_.isDirectory)
      .flatMap(x => x.listFiles())
      .filter(_.getName.endsWith(".json"))
      .map(readFingerprint)

  def readFingerprint(file: File): PPMFingerprint =
    Source.fromFile(file).mkString.parseJson.convertTo[PPMFingerprint]

  //fingerprints.foreach(println)

  def rotationFor(segment: SegmentReference): Double =
    fingerprints.find(_.segment == segment).get.zDirection

  val allZ =
    fingerprints.flatMap { p =>
      p.radar
        .filter(_.entries.nonEmpty)
        .map(_.z)
    }

  val radar10000 =
    fingerprints
      .flatMap { p =>
        p.radar
          .find(l => l.z == 10000 && l.entries.nonEmpty).toSeq
          .flatMap(_.entries)
          .map(p.segment -> _)
      }

  val byTheta =
    radar10000.groupBy(x => math.round(x._2.theta).toInt)

  /*byTheta.foreach {
    case (theta, entries) =>
      println(s"Theta $theta Num: ${entries.size}")
      entries.sortBy(_._2.r).foreach {
        case (segment, entry) =>
          println(s"  $segment: $entry")
      }
  }*/
  val entries = byTheta(225)

  val maxThickness = 10

  type E = (SegmentReference, RadarEntry)
  case class State(
      collected:  Seq[Seq[E]],
      currentEls: Seq[E]
  ) {
    def avgR: Float = currentEls.map(_._2.r).sum / currentEls.size

    def next(e: E): State =
      if (!currentEls.exists(_._1.segmentId == e._1.segmentId) && (currentEls.isEmpty || math.abs(e._2.r - avgR) < maxThickness))
        copy(currentEls = currentEls :+ e)
      else
        copy(collected = collected :+ currentEls, currentEls = Seq(e))

    def finish: Seq[Seq[E]] = collected :+ currentEls
  }

  val initialState = State(Nil, Nil)
  val grouped =
    entries
      .toVector
      .sortBy(_._2.r)
      .foldLeft(initialState)(_.next(_)).finish

  /*grouped.zipWithIndex.foreach {
    case (els, idx) =>
      println(s"[$idx] Avg R: ${els.map(_._2.r).sum / els.size}")
      els.foreach {
        case (segment, entry) =>
          println(s"  $segment: $entry")
      }
  }*/
  val globalTheta =
    grouped.zipWithIndex.map {
      case (entries, idx) =>
        (entries, 360 * idx + 225)
    }

  globalTheta.foreach {
    case (entries, theta) =>
      println(s"Theta $theta Num: ${entries.size}")
      entries.sortBy(_._2.r).foreach {
        case (segment, entry) =>
          println(s"  $segment: $entry")
      }
  }

  val distanceMap =
    globalTheta.sliding(2).map {
      case Seq((e1, th1), (e2, th2)) =>
        // match entries from e1 and e2
        val intersect = e1.map(_._1).toSet.intersect(e2.map(_._1).toSet)

        val dist =
          if (intersect.isEmpty) {
            println(s"Hard to scale because no overlap between $th1 and $th2")
            // assume a circle around the center
            // this will overestimate the size of the circle, but that's ok
            // this assumes that u/v and x/y/z are not scaled
            e2.head._2.r * 2 * math.Pi
          } else {
            intersect.map { seg =>
              val el1 = e1.find(_._1 == seg).get._2
              val el2 = e2.find(_._1 == seg).get._2

              val du = el2.u - el1.u
              val dv = el2.v - el1.v
              val dist = math.sqrt(du * du + dv * dv)

              println(f"[$th1%3d] ${seg.segmentId} $el1%20s [$th2%3d] $el2%20s dist: $dist%5.2f du: $du dv: $dv")
              dist
            }.sum / intersect.size
          }
        (th1, th2, dist)
    }.toVector

  val thetaToX =
    distanceMap.scanLeft(725d) {
      case (acc, (_, _, dist)) => acc + dist
    }.zip(distanceMap.map(_._1)).map(_.swap)

  thetaToX.foreach(println)

  /*Future.traverse(parts)(p => imageInfo(p.segment).map(p -> _)).await { parts =>
    val p1 = parts.flatMap {
      case (p, Some(info)) =>
  val newX = if (p.flip) p.x - info.width * math.cos(p.rotation / 360 * 2 * math.Pi) else p.x
  println(f"Oldx: ${p.x} NewX: $newX%.2f segment: ${p.segment}")
  Seq(p.copy(width = info.width, height = info.height, x = newX))

      case _ => Nil*/

  def debugImageFor(segment: SegmentReference, to: File): File = {
    val info = segmentInfos(segment)
    import segment._
    println(info)

    val img = new BufferedImage(info.width, info.height, BufferedImage.TYPE_INT_ARGB)
    val img2 = ImageIO.read(new File(s"../data/raw/scroll$scroll/$segmentId/mask.png"))

    val g = img.createGraphics()
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, info.width, info.height)
    val oldComp = g.getComposite
    //g.setComposite(AlphaComposite.DstOut)

    g.setXORMode(java.awt.Color.BLACK)
    g.drawImage(img2, 0, 0, null)
    g.setComposite(oldComp)
    // enable antialiasing
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)

    g.setColor(java.awt.Color.RED)
    //g.setFont(Font.)
    g.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 300))

    def withRotate(x: Int, y: Int)(f: => Unit): Unit = {
      val oldTransform = g.getTransform
      g.rotate(rotationFor(segment) / 360 * 2 * math.Pi, x, y)
      f
      g.setTransform(oldTransform)
    }

    val radius = 100

    globalTheta
      .flatMap(x => x._1.map(_ -> x._2))
      .groupBy(_._1._1).toVector
      .filter(_._1 == segment)
      .flatMap(_._2)
      .foreach {
        case ((_, entry), global) =>
          val x = entry.u
          val y = entry.v
          g.fillOval(x - radius, y - radius, radius * 2, radius * 2)

          withRotate(x, y) {
            g.drawString(f"${global}%5.2f / ${entry.theta}%5.2f", x + radius + 5, y)
          }
      }

    g.dispose()

    ImageIO.write(img, "png", to)
    to
  }

  lazy val imageParts: Seq[ImagePart] = {
    globalTheta
      .flatMap(x => x._1.map(_ -> x._2))
      .groupBy(_._1._1).toVector
      .sortBy(_._2.map(_._2).min)
      //.filter(_._1.segmentId.contains("151000"))
      //.filter(x => x._1.segmentId.contains("2745") || x._1.segmentId.contains("0314") /*|| x._1.segmentId.contains("3333")*/ )
      .flatMap {
        case (segment: SegmentReference, entries: Seq[((SegmentReference, RadarEntry), Int)]) =>
          val es = entries.map(x => x._1._2 -> x._2).sortBy(_._2)

          if (es.size < 2) {
            println(s"Segment $segment has only ${es.size} entries")
            Nil
          } else {
            val e1 = es.head._1
            val t1 = es.head._2

            val e2 = es.last._1
            val t2 = es.last._2

            val du = e2.u - e1.u
            val dv = e2.v - e1.v

            val flip = dv < 0

            val width = segmentInfos(segment).width
            val height = segmentInfos(segment).height

            val tx = -thetaToX.find(_._1 == t1).get._2
            val rotate =
              if (!flip)
                math.Pi - math.atan2(dv, du)
              else
                math.atan2(dv, du)

            val protate = rotate // math.Pi / 2 //2 * math.Pi - rotate

            val centerU = width / 2
            val centerV = height / 2

            val u = if (flip) width - e1.u else e1.u

            val nu = (u - centerU) * math.cos(protate) - (e1.v - centerV) * math.sin(protate) + centerU
            val nv = (u - centerU) * math.sin(protate) + (e1.v - centerV) * math.cos(protate) + centerV

            val u1 = tx - nu
            val v1 = -10000 - nv

            println(f"Segment $segment: e1: $e1 t1: $t1 du: $du%5.2f dv: $dv%5.2f rotate: ${rotate / 2 / math.Pi * 360}%5.2f tx: $tx%5.2f nu: $nu%5.2f nv: $nv%5.2f v: ${e1.v}")

            import segment._
            Seq(
              ImagePart(
                segment,
                s"/scroll/$scroll/segment/$segmentId/2999",
                u1, //u1, //-tx + nu,
                v1,
                protate / 2 / math.Pi * 360, //rotate / 2 / math.Pi * 360,
                width, 0, flip
              /*-tx + nu,
                14000 - 10000 + nv,
                180 - rotate / 2 / math.Pi * 360,
                segmentInfos(segment).width, 0, flip*/ )
            )
          }

        //val theta = entries.map(_._2).sum / entries.size
        //val x = thetaToX.find(_._1 == theta).get._2
        //ImagePart(segment, x, entries.map(_._1._2))
      } //.take(1) //.drop(1).take(1)
  }
}
