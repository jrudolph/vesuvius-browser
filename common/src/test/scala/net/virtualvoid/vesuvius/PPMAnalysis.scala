package net.virtualvoid.vesuvius

import java.io.File
import spray.json.*

import scala.io.Source

object PPMAnalysis extends App {
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

}
