package net.virtualvoid.vesuvius

import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.apache.pekko.http.scaladsl.model.headers.HttpCookie
import org.apache.pekko.http.scaladsl.server.{ Directive, Directive0, Directive1, Route }
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.parboiled2.util.Base64
import spray.json.*

import javax.crypto.{ Cipher, SecretKeyFactory }
import javax.crypto.spec.{ GCMParameterSpec, PBEKeySpec, SecretKeySpec }

case class LoggedInUser(name: String, admin: Boolean, validUntil: Long)
object LoggedInUser {
  import DefaultJsonProtocol._

  implicit val format: RootJsonFormat[LoggedInUser] = jsonFormat3(LoggedInUser.apply)
}

trait UserManagement {
  def loggedIn: Directive1[Option[LoggedInUser]]
  def ensureAdmin: Directive0
  def adminUser: Directive1[LoggedInUser] = loggedIn.flatMap {
    case Some(user) if user.admin => provide(user)
    case _                        => redirect("/login", StatusCodes.Found)
  }
  def login(name: String, password: String): Route
  def logout: Route
}

object UserManagement {
  def apply(config: AppConfig): UserManagement = new UserManagement {
    val secretKey = {
      val iterationCount = 1000 // Adjust as needed for your security requirements
      val keyLength = 256 // Key length in bits

      // Create the key specification
      val keySpec = new PBEKeySpec(config.serverKey.toCharArray, new Array[Byte](16), iterationCount, keyLength)

      // Generate the derived key using PBKDF2
      val keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
      val derivedKey = keyFactory.generateSecret(keySpec).getEncoded

      // Create a SecretKeySpec from the derived key
      new SecretKeySpec(derivedKey, "AES")
    }

    val defaultValidityMillis = 24 * 60 * 60 * 1000L

    def loggedIn: Directive1[Option[LoggedInUser]] =
      cookie("ves_user").flatMap { pair =>
        try {
          val user = decrypt[LoggedInUser](pair.value)
          provide(Some(user).filter(_.validUntil > System.currentTimeMillis()))
        } catch {
          case _: Throwable => deleteCookie("ves_user") & provide(None)
        }
      }
        .recover(_ => provide(None))

    def ensureAdmin: Directive0 =
      loggedIn.flatMap { user =>
        if (user.exists(_.admin)) pass
        else redirect("/login", StatusCodes.Found)
      }

    def login(name: String, password: String): Route =
      if (password == config.adminPassword) {
        val user = LoggedInUser(name, admin = true, validUntil = System.currentTimeMillis() + defaultValidityMillis)
        val cookie = encrypt(user)
        setCookie(HttpCookie("ves_user", cookie, maxAge = Some(defaultValidityMillis / 1000))) {
          redirect("/", StatusCodes.Found)
        }
      } else {
        redirect("/login", StatusCodes.Found)
      }

    def logout: Route =
      deleteCookie("ves_user") {
        redirect("/", StatusCodes.Found)
      }

    def encrypt[T: JsonFormat](t: T): String = {
      val (enc, iv) = encrypt(t.toJson.compactPrint.getBytes("UTF-8"))
      Seq(enc, iv).map(Base64.rfc2045().encodeToString(_, false)).mkString(":")
    }

    def decrypt[T: JsonFormat](s: String): T = {
      val Array(enc, iv) = s.split(":").map(s => Base64.rfc2045().decode(s.toCharArray))
      decrypt(enc, iv).parseJson.convertTo[T]
    }

    def decrypt(data: Array[Byte], iv: Array[Byte]): String = {
      val cipher = aes()
      cipher.init(Cipher.DECRYPT_MODE, secretKey, gcmParams(iv))
      val output = cipher.doFinal(data)

      new String(output, "UTF-8")
    }

    def encrypt(data: Array[Byte]): (Array[Byte], Array[Byte]) = {
      val cipher = aes()
      val iv = newIV()
      cipher.init(Cipher.ENCRYPT_MODE, secretKey, gcmParams(iv))
      val output = cipher.doFinal(data)
      (output, iv)
    }

    def aes(): Cipher = javax.crypto.Cipher.getInstance("AES/GCM/NoPadding")
    def gcmParams(iv: Array[Byte]): GCMParameterSpec = new GCMParameterSpec(128, iv)

    def newIV(): Array[Byte] = {
      val iv = new Array[Byte](16)
      val random = new java.security.SecureRandom()
      random.nextBytes(iv)
      iv
    }
  }
}
