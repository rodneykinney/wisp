package wisp

import spray.json.JsValue

/**
 * Created by rodneykinney on 4/18/15.
 */
trait CustomJsonObject extends Product {
  val other: Map[String, JsValue]
}
