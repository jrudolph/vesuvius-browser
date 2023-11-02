package net.virtualvoid.vesuvius

import spray.json._

trait SprayJsonHelpers {
  implicit class JsObjectExtension(obj: JsObject) {
    def +(field: (String, JsValue)): JsObject =
      JsObject(obj.fields + field)
  }
}
