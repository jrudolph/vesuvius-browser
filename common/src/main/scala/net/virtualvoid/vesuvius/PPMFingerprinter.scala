package net.virtualvoid.vesuvius

import java.io.File

case class Span(min: Int, max: Int)

case class RadarEntry(
    theta: Float,
    r:     Float,
    u:     Int,
    v:     Int,
    x:     Int,
    y:     Int
)

case class RadarLayer(
    z:       Int,
    entries: Seq[RadarEntry]
)

case class PPMFingerprint(
    segment:    SegmentReference,
    xSpan:      Span,
    ySpan:      Span,
    zSpan:      Span,
    zDirection: Float,
    radar:      Seq[RadarLayer]
)
object PPMFingerprint {
  import spray.json._
  import DefaultJsonProtocol._

  implicit val spanFormat: JsonFormat[Span] = jsonFormat2(Span.apply)
  implicit val radarEntryFormat: JsonFormat[RadarEntry] = jsonFormat6(RadarEntry.apply)
  implicit val radarLayerFormat: JsonFormat[RadarLayer] = jsonFormat2(RadarLayer.apply)
  implicit val ppmFingerprintFormat: JsonFormat[PPMFingerprint] = jsonFormat6(PPMFingerprint.apply)
}

object PPMFingerprinter {
  def fingerprint(segment: SegmentReference, ppmFile: File): PPMFingerprint = {
    val ppm = PPMReader(ppmFile)

    val zDir = findZDirection(ppm)
    val (xSpan, ySpan, zSpan) = calculateBoundingBox(ppm)
    val radar = calculateRadar(ppm)

    PPMFingerprint(segment, xSpan, ySpan, zSpan, zDir, radar)
  }

  def findZDirection(implicit reader: PPMReader): Float = {
    import reader.{ width, height }

    def findRotationAngle(uv: UV, r: Int, n: Int): Double = {
      // FIXME: result sign and direction is a bit weird since u/v does not use conventional coordinate system (v points down)
      // 0 degrees is to the right, 90 degrees is down

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
    val avgAngle = math.atan2(dirSum._1, dirSum._2).toFloat
    println(f"Avg angle: ${avgAngle / 2 / math.Pi * 360}%7.4f")
    avgAngle
  }
  def calculateBoundingBox(implicit reader: PPMReader): (Span, Span, Span) = {
    case class State(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int) {
      def update(uv: UV): State =
        State(
          math.min(minX, uv.x),
          math.max(maxX, uv.x),
          math.min(minY, uv.y),
          math.max(maxY, uv.y),
          math.min(minZ, uv.z),
          math.max(maxZ, uv.z)
        )
    }
    val s = reader.uvs.foldLeft(State(Int.MaxValue, 0, Int.MaxValue, 0, Int.MaxValue, 0))(_.update(_))
    (Span(s.minX, s.maxX), Span(s.minY, s.maxY), Span(s.minZ, s.maxZ))
  }

  def calculateRadar(implicit reader: PPMReader): Seq[RadarLayer] = {
    import reader.uvs

    // traces found with distance smaller than below are folded together
    val minLayerThickness = 10

    def at(z: Int): RadarLayer = {
      val (centerX, centerY) = scroll1Center(z)

      val entries: Iterable[(Float, Double, UV)] =
        uvs
          .filter(_.z == z)
          .flatMap { uv =>
            val theta = math.atan2(uv.x - centerX, uv.y - centerY) + math.Pi
            val r = math.hypot(uv.x - centerX, uv.y - centerY)
            val thetaQ = math.round(theta / 2f / math.Pi * 720)

            if (thetaQ.toInt % 90 == 0) {
              println(f"At $uv theta=${thetaQ / 2}%7.4f r=$r%7.4f")
              Some((thetaQ / 2 % 360, r, uv))
            } else
              None
          }

      def combineUV(uv1: UV, uv2: UV): UV =
        UV((uv1.u + uv2.u) / 2, (uv1.v + uv2.v) / 2)

      // we try to collect adjacent entries with rs that are close together (minLayerThickness)
      // and calculate average r and uv for them
      type E = (Double, UV)
      def simplify(entries: Seq[(Double, UV)]): Seq[(Double, UV)] =
        (entries :+ (100000d, UV(0, 0))).scanLeft[(E, Option[E])](((0.0, UV(0, 0)) -> Option.empty[E]): (E, Option[E])) { (cur: (E, Option[E]), next: E) =>
          if (math.abs(cur._1._1 - next._1) < minLayerThickness) (((cur._1._1 + next._1) / 2, combineUV(cur._1._2, next._2)), None)
          else (next, Some(cur._1))
        }.flatMap(_._2).drop(1)

      val radarEntries =
        entries.groupBy(_._1)
          .toVector
          .flatMap {
            case (theta, entries) =>
              simplify(entries.map(x => (x._2, x._3)).toVector.sortBy(_._1)).map(x => (theta, x._1, x._2))
          }
          .map {
            case (theta, r, uv) =>
              val res = RadarEntry(theta, r.toFloat, uv.u, uv.v, uv.x, uv.y)
              println(res)
              res
          }

      RadarLayer(z, radarEntries)
    }

    scroll1Center.keys.toVector.sorted.map(at)
  }

  // center of the scroll in different z-layers in (z,x,y)
  lazy val scroll1Center: Map[Int, (Int, Int)] =
    """z,center_x,center_y
      |1000,3943,2363
      |1500,3892,2285
      |2000,3860,2255
      |2500,3826,2227
      |3000,3804,2212
      |3500,3789,2238
      |4000,3738,2227
      |4500,3766,2393
      |5000,3778,2587
      |5500,3754,2816
      |6000,3782,3097
      |6500,3742,3341
      |7000,3718,3455
      |7500,3631,3470
      |8000,3584,3414
      |8500,3513,3426
      |9000,3379,3511
      |9500,3283,3538
      |10000,3273,3589
      |10500,3199,3726
      |11000,3132,3850
      |11500,3060,4006
      |12000,2985,4135
      |12500,2942,4309
      |13000,2881,4466
      |13500,2854,4668""".stripMargin
      .split("\n")
      .drop(1)
      .map(_.split(","))
      .map { case Array(z, x, y) => z.toInt -> (x.toInt, y.toInt) }
      .toMap
}
