package net.virtualvoid.vesuvius

import org.apache.pekko.http.scaladsl.marshalling.{ Marshaller, ToEntityMarshaller }
import org.apache.pekko.http.scaladsl.model.MediaType
import org.apache.pekko.http.scaladsl.model.MediaTypes.*
import play.twirl.api.{ Html, Txt, Xml }

// copied from https://github.com/btomala/akka-http-twirl/blob/master/src/main/scala/akkahttptwirl/TwirlSupport.scala
object TwirlSupport extends TwirlSupport

trait TwirlSupport {

  /** Serialize Twirl `Html` to `text/html`. */
  implicit val twirlHtmlMarshaller: ToEntityMarshaller[Html] = twirlMarshaller[Html](`text/html`)

  /** Serialize Twirl `Txt` to `text/plain`. */
  implicit val twirlTxtMarshaller: ToEntityMarshaller[Txt] = twirlMarshaller[Txt](`text/plain`)

  /** Serialize Twirl `Xml` to `text/xml`. */
  implicit val twirlXmlMarshaller: ToEntityMarshaller[Xml] = twirlMarshaller[Xml](`text/xml`)

  /** Serialize Twirl formats to `String`. */
  protected def twirlMarshaller[A <: AnyRef: Manifest](contentType: MediaType): ToEntityMarshaller[A] =
    Marshaller.StringMarshaller.wrap(contentType)(_.toString)

}