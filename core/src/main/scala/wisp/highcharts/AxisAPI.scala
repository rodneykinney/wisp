package wisp.highcharts

import spray.json.{JsonWriter, JsValue}
import wisp.CustomJsonObject

import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
case class Axis(
                 title: AxisTitle = AxisTitle(),
                 `type`: AxisType = AxisType.linear,
                 categories: IndexedSeq[String] = null,
                 min: Option[Double] = None,
                 max: Option[Double] = None,
                 other: Map[String, JsValue] = Map()) extends CustomJsonObject {
  def api[T](update: Axis => T) = new AxisAPI(this)(update)
}

class AxisAPI[T](axis: Axis)(update: Axis => T) extends API {
  @WebMethod
  def axisType(x: AxisType) = update(axis.copy(`type` = x))

  @WebMethod
  def title = axis.title.api(t => update(axis.copy(title = t)))

  @WebMethod
  def categories(x: Iterable[String]) = update(axis.copy(categories = x.toIndexedSeq))

  @WebMethod
  def range(min: Double, max: Double) = update(axis.copy(min = Some(min), max = Some(max)))

  @WebMethod(action = "Add additional values to the JSON object")
  def addOption[V: JsonWriter](name: String, value: V)
  = update(axis.copy(other = axis.other + (name -> implicitly[JsonWriter[V]].write(value))))

}

case class AxisTitle(text: String = "",
                     other: Map[String, JsValue] = Map()) extends CustomJsonObject {
  def api[T](update: AxisTitle => T) = new AxisTitleAPI(this, update)
}

class AxisTitleAPI[T](at: AxisTitle, update: AxisTitle => T) extends API {
  @WebMethod
  def text(x: String) = update(at.copy(text = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def addOption[V: JsonWriter](name: String, value: V)
  = update(at.copy(other = at.other + (name -> implicitly[JsonWriter[V]].write(value))))
}


