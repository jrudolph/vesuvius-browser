package net.virtualvoid.vesuvius

import java.io.File
import spray.json.*

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

case class Point2D(
    x: Int,
    y: Int
)

case class CrosscutLines(
    p0: Point2D,
    p1: Point2D
)

case class SegmentCrosscut(
    z:     Int,
    lines: Seq[CrosscutLines]
)

case class SegmentCrosscutReport(
    segment:   SegmentReference,
    minZ:      Int,
    maxZ:      Int,
    crosscuts: Seq[SegmentCrosscut]
)
object SegmentCrosscutReport {
  import DefaultJsonProtocol._

  implicit val point2DFormat: JsonFormat[Point2D] = jsonFormat2(Point2D.apply)
  implicit val crosscutPathElementFormat: JsonFormat[CrosscutLines] = jsonFormat2(CrosscutLines.apply)
  implicit val segmentCrosscutFormat: JsonFormat[SegmentCrosscut] = jsonFormat2(SegmentCrosscut.apply)
  implicit val segmentCrosscutReportFormat: JsonFormat[SegmentCrosscutReport] = jsonFormat4(SegmentCrosscutReport.apply)
}

object CrossCutter {
  val StepSize = 500
  def fromObj(segmentReference: SegmentReference, objFile: File): SegmentCrosscutReport = {
    val mesh = Mesh.fromObjFile(objFile)
    val zBounds = mesh.vertices.map(_.z).min.toInt to mesh.vertices.map(_.z).max.toInt

    def crosscut(z: Int): SegmentCrosscut = {
      val pathElements: Seq[CrosscutLines] =
        mesh.faces.flatMap { face =>
          val v1 = mesh.vertices(face.vertices._1 - 1)
          val v2 = mesh.vertices(face.vertices._2 - 1)
          val v3 = mesh.vertices(face.vertices._3 - 1)

          val minz = v1.z.min(v2.z).min(v3.z)
          val maxz = v1.z.max(v2.z).max(v3.z)

          if (minz < z && maxz > z) {
            val points = Seq.newBuilder[Point2D]

            def intersects(v1: Vec3, v2: Vec3, w: Int): Boolean =
              (v1.z - w).sign != (v2.z - w).sign

            def addIntersectionPoints(v1: Vec3, v2: Vec3): Unit =
              if (intersects(v1, v2, z)) {
                val x1 = v1.x
                val y1 = v1.y

                val x2 = v2.x
                val y2 = v2.y

                val d1 = z - v1.z
                val d2 = z - v2.z

                val t = d1 / (d1 - d2)

                val x = x1 + t * (x2 - x1)
                val y = y1 + t * (y2 - y1)

                points += Point2D(x.toInt, y.toInt)
              }

            addIntersectionPoints(v1, v2)
            addIntersectionPoints(v2, v3)
            addIntersectionPoints(v3, v1)

            if (points.result().size == 2) Seq(CrosscutLines(points.result().head, points.result().last))
            else Seq.empty
          } else Seq.empty
        }

      SegmentCrosscut(z, pathElements)
    }

    val cuts =
      ((zBounds.min + StepSize - 1) / StepSize * StepSize to zBounds.max by StepSize).headOption.toSeq map crosscut

    SegmentCrosscutReport(segmentReference, zBounds.min, zBounds.max, cuts)
  }

  def paintLines(lines: Seq[(Point2D, Point2D)]): Unit = {
    val bufferedImage = new BufferedImage(5000, 5000, BufferedImage.TYPE_INT_RGB)
    val g = bufferedImage.createGraphics()
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, 5000, 5000)
    g.setColor(java.awt.Color.BLACK)
    lines.foreach {
      case (Point2D(x1, y1), Point2D(x2, y2)) =>
        g.drawLine(x1, y1, x2, y2)
    }

    ImageIO.write(bufferedImage, "png", new File("/tmp/crosscut.png"))
  }
}
