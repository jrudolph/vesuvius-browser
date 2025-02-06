package net.virtualvoid

package object vesuvius extends PekkoHttpHelpers {
  val CacheBusterId = "v2"

  implicit class StringExtension(val s: String) extends AnyVal {
    def sha256sum: String = {
      import java.security.MessageDigest
      import java.math.BigInteger
      val md = MessageDigest.getInstance("SHA-256")
      val digest = md.digest(s.getBytes("UTF-8"))
      val bigInt = new BigInteger(1, digest)
      bigInt.toString(16)
    }
  }
}
