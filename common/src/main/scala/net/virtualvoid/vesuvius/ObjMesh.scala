package net.virtualvoid.vesuvius

import java.io.File
import scala.io.Source

case class Vec3(x: Double, y: Double, z: Double)
case class Vec2(x: Double, y: Double)

case class Face(
    vertices:  (Int, Int, Int),
    normals:   (Int, Int, Int),
    texCoords: (Int, Int, Int)
)

case class Mesh(
    vertices:  IndexedSeq[Vec3],
    normals:   IndexedSeq[Vec3],
    texCoords: IndexedSeq[Vec2],
    faces:     IndexedSeq[Face]
)
object Mesh {
  def fromObjFile(objFile: File): Mesh = {
    val vertices = Vector.newBuilder[Vec3]
    val normals = Vector.newBuilder[Vec3]
    val texCoords = Vector.newBuilder[Vec2]
    val faces = Vector.newBuilder[Face]

    val FPR = """-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?"""
    val IntR = """\d+"""

    val VRegEx = s"""v\\s+($FPR)\\s+($FPR)\\s+($FPR)""".r
    val VNRegEx = s"""vn\\s+($FPR)\\s+($FPR)\\s+($FPR)""".r
    val VTRegEx = s"""vt\\s+($FPR)\\s+($FPR)""".r
    val FRegEx = s"""f\\s+($IntR)/($IntR)/($IntR)\\s+($IntR)/($IntR)/($IntR)\\s+($IntR)/($IntR)/($IntR)""".r

    Source.fromFile(objFile).getLines().map(_.trim).foreach {
      case VRegEx(x, y, z)  => vertices += Vec3(x.toDouble, y.toDouble, z.toDouble)
      case VNRegEx(x, y, z) => normals += Vec3(x.toDouble, y.toDouble, z.toDouble)
      case VTRegEx(x, y)    => texCoords += Vec2(x.toDouble, y.toDouble)
      case FRegEx(v1, vt1, vn1, v2, vt2, vn2, v3, vt3, vn3) =>
        faces += Face(
          (v1.toInt, v2.toInt, v3.toInt),
          (vn1.toInt, vn2.toInt, vn3.toInt),
          (vt1.toInt, vt2.toInt, vt3.toInt)
        )
      case x =>
      //println(s"Ignoring line: $x")
    }

    Mesh(vertices.result(), normals.result(), texCoords.result(), faces.result())
  }
}
