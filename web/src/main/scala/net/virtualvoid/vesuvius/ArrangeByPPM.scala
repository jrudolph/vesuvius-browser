package net.virtualvoid.vesuvius

import java.io.File
import spray.json.*

import java.awt.{AlphaComposite, BasicStroke, Color, Graphics2D}
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
      .filter(fp => segmentInfos.contains(fp.segment))

  def readFingerprint(file: File): PPMFingerprint =
    try Source.fromFile(file).mkString.parseJson.convertTo[PPMFingerprint]
    catch {
      case e: Exception =>
        println(s"Error reading $file: $e")
        throw e
    }

  //fingerprints.foreach(println)

  def rotationFor(segment: SegmentReference): Double =
    fingerprints.find(_.segment == segment).get.zDirection

  val allZ =
    fingerprints.flatMap { p =>
      p.radar
        .filter(_.entries.nonEmpty)
        .map(_.z)
    }

  val mainZ = 7000

  val radarMainZ =
    fingerprints
      .flatMap { p =>
        p.radar
          .find(l => l.z == mainZ && l.entries.nonEmpty).toSeq
          .flatMap(_.entries)
          .map(p.segment -> _)
      }

  val byTheta =
    radarMainZ.groupBy(x => math.round(x._2.theta).toInt)

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
    val img2 = ImageIO.read(new File(s"../data/raw/scroll$scrollId/$segmentId/mask.png"))

    val g = img.createGraphics()
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    //g.setColor(new Color(0,0,0,0))
    //g.fillRect(0, 0, info.width, info.height)
    val oldComp = g.getComposite
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER))
    //g.setComposite(AlphaComposite.DstOut)

    //g.setXORMode(Color.BLACK)
    //g.drawImage(img2, 0, 0, null)
    //g.setComposite(oldComp)
    // enable antialiasing

    g.setColor(Color.RED)
    //g.setFont(Font.)
    g.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 300))

    def withRotate(x: Int, y: Int)(f: => Unit): Unit = {
      val oldTransform = g.getTransform
      g.rotate(rotationFor(segment) / 360 * 2 * math.Pi, x, y)
      f
      g.setTransform(oldTransform)
    }

    val radius = 70

    val globalPoints =
    globalTheta
      .flatMap(x => x._1.map(_ -> x._2))
      .groupBy(_._1._1).toVector
      .filter(_._1 == segment)
      .flatMap(_._2)

    def withThickStroke(thickness: Int)(f: => Unit): Unit = {
      val origStroke = g.getStroke
      val thickStroke = new BasicStroke(thickness)
      g.setStroke(thickStroke)
      f
      g.setStroke(origStroke)
    }

    println(s"Drawing global for $segment")
    globalPoints
      .foreach {
        case ((_, entry), global) =>
          val x = entry.u
          val y = entry.v
          g.setColor(Color.RED)
          g.fillOval(x - radius, y - radius, radius * 2, radius * 2)

          withRotate(x, y) {
            g.drawString(f"${global}%5.0f° / ${entry.theta}%3.0f° / z = ${mainZ}%d", x + radius + 5, y)
          }

          // draw +
          g.setColor(Color.BLACK)
          withThickStroke(20) {
            g.drawLine(x, y - radius, x, y + radius)
            g.drawLine(x - radius, y, x + radius, y)
          }
      }

    g.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 100))
    println(s"Drawing local for $segment")
    val local = fingerprints.find(_.segment == segment).get
    local.radar
      .flatMap(l => l.entries.map(l.z -> _))
      .filterNot { case (z, e) => globalPoints.exists(_._1._2 == e) }
      .filter(_._1 % 1000 == 0)
      .foreach { case (z, entry) =>
        val x = entry.u
        val y = entry.v
        val radius = 20

        g.setColor(Color.getHSBColor(entry.theta / 360, 1, 1))
        g.fillOval(x - radius, y - radius, radius * 2, radius * 2)

        withRotate(x, y) {
          g.drawString(f"${entry.theta}%3.0f° / z = $z%d", x + radius + 5, y)
        }

        g.setColor(Color.BLACK)
        withThickStroke(5) {
          g.drawLine(x, y - radius, x, y + radius)
          g.drawLine(x - radius, y, x + radius, y)
        }
      }

    g.dispose()

    ImageIO.write(img, "png", to)
    to
  }

  def rotateFlip(segment: SegmentReference): (Double, Boolean) = {
    val fp = fingerprints.find(_.segment == segment).get
    val spans =
      fp.radar
        .flatMap(r => r.entries.map(r.z -> _))
        .groupBy(e => (e._1, e._2.theta))
        .filter(_._2.size > 1)

    if (spans.isEmpty) {
      println(s"Segment $segment has no spans, cannot determine flip")
      (math.Pi / 2 - fp.zDirection, false)
    } else {
      val entries = spans.maxBy(_._2.size)._2.map(_._2)
      val e1 = entries.maxBy(_.r)
      val e2 = entries.minBy(_.r)

      val du = e2.u - e1.u
      val dv = e2.v - e1.v

      val flip = dv < 0

      val rotatedvdu = math.atan2(dv, du)

      val rotateFlipdvdu =
        if (!flip)
          math.Pi - rotatedvdu
        else
          rotatedvdu
      (rotateFlipdvdu, flip)
    }
  }

  lazy val imageParts: Seq[ImagePart] = {
    globalTheta
      .flatMap(x => x._1.map(_ -> x._2))
      .groupBy(_._1._1).toVector
      .sortBy(_._2.map(_._2).min)
      //.filter(_._1.segmentId.contains("151000"))
      //.filter(x => x._1.segmentId.contains("2745") || x._1.segmentId.contains("0314") || x._1.segmentId.contains("1847"))
      .flatMap {
        case (segment: SegmentReference, entries: Seq[((SegmentReference, RadarEntry), Int)]) =>
          val es = entries.map(x => x._1._2 -> x._2).sortBy(_._2)

          val e1 = es.head._1
          val t1 = es.head._2

          val width = segmentInfos(segment).width
          val height = segmentInfos(segment).height
          thetaToX.find(_._1 == t1).map(-_._2).flatMap { tx =>

            val (rotate: Double, flip: Boolean) =
              if (es.size < 2) {
                rotateFlip(segment)
                /*println(s"Segment $segment has only ${es.size} entries")
                val flip = false
                val rotate0 = (math.Pi / 2 - rotationFor(segment))
                val rotate =
                  if (!flip)
                    rotate0
                  else
                    -rotate0

                (rotate, flip)*/
              } else {
                val e2 = es.last._1
                val t2 = es.last._2

                val du = e2.u - e1.u
                val dv = e2.v - e1.v

                val flip = dv < 0

                val rotatedvdu = math.atan2(dv, du)

                val rotateFlipdvdu =
                  if (!flip)
                    math.Pi - rotatedvdu
                  else
                    rotatedvdu

                (rotateFlipdvdu, flip)
              }

            val protate = rotate // math.Pi / 2 //2 * math.Pi - rotate

            val centerU = width / 2
            val centerV = height / 2

            val u = if (flip) width - e1.u else e1.u

            val nu = (u - centerU) * math.cos(protate) - (e1.v - centerV) * math.sin(protate) + centerU
            val nv = (u - centerU) * math.sin(protate) + (e1.v - centerV) * math.cos(protate) + centerV

            val u1 = tx - nu
            val v1 = -mainZ - nv

            //println(f"Segment $segment: e1: $e1 t1: $t1 du: $du%5.2f dv: $dv%5.2f flip: $flip rotate0: ${rotate0 * 360 / 2 / math.Pi}%5.2f rotateFlipdvdu: ${rotateFlipdvdu * 360 / 2 / math.Pi}%5.2f rotatedvdu: ${rotatedvdu * 360 / 2 / math.Pi}%5.2f zDir: ${rotationFor(segment) * 360 / 2 / math.Pi}%5.2f rotate: ${rotate / 2 / math.Pi * 360}%5.2f tx: $tx%5.2f nu: $nu%5.2f nv: $nv%5.2f v: ${e1.v}")

            //val targetLayer = if (flip) "2343" else "2342"
            val targetLayer = 2999

            import segment._
            Some(
              ImagePart(
                segment,
                s"/scroll/$scrollId/segment/$segmentId/$targetLayer",
                u1, //u1, //-tx + nu,
                v1,
                protate / 2 / math.Pi * 360, //rotate / 2 / math.Pi * 360,
                width, 0, flip
              )
            )
          }
      } //.take(1) //.drop(1).take(1)
  }

  fingerprints
    .filter(!_.radar.exists(x => x.entries.nonEmpty && x.z == mainZ))
    .sortBy(_.segment.segmentId)
    .filter(fp => segmentInfos.contains(fp.segment))
    .foreach { fp =>
      val info = segmentInfos(fp.segment)
      println(s"Segment ${fp.segment} has no radar at z=$mainZ bounds: ${fp.zSpan} width: ${info.width} height: ${info.height} area: ${info.width * info.height / 1_000_000} MP")

    }
}
