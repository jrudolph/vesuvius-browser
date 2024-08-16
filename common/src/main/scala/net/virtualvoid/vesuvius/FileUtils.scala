package net.virtualvoid.vesuvius

import java.io.File

object FileUtils {
  def firstFileNameMatching(dir: File, pattern: String): Option[File] = {
    val files = dir.listFiles()
    if (files == null) None
    else files.find(_.getName.matches(pattern))
  }
}
