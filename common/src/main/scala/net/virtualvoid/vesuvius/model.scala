package net.virtualvoid.vesuvius

case class SegmentReference(scroll: Int, segmentId: String, base: ScrollServerBase) {
  def baseUrl: String = s"${base.baseUrl(scroll)}$segmentId/"
}

sealed trait ScrollServerBase {
  def baseUrl(scroll: Int): String
}

case object FullScrollsBase extends ScrollServerBase {
  def baseUrl(scroll: Int): String =
    s"http://dl.ash2txt.org/full-scrolls/Scroll$scroll.volpkg/paths/"
}

case object HariSeldonUploadsBase extends ScrollServerBase {
  def baseUrl(scroll: Int): String =
    s"http://dl.ash2txt.org/hari-seldon-uploads/team-finished-paths/scroll$scroll/"
}

case class ImageInfo(
    ref:    SegmentReference,
    width:  Int,
    height: Int,
    area:   Option[Float]
) {
  def scroll: Int = ref.scroll
  def segmentId: String = ref.segmentId
}