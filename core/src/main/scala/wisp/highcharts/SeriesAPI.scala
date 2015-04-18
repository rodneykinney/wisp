package wisp.highcharts

import spray.json.JsValue
import wisp.CustomJsonObject

import java.awt.Color
import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */


case class Series(
                   data: Seq[Point],
                   name: String = "",
                   `type`: SeriesType,
                   other: Map[String, JsValue] = Map()
                   ) extends CustomJsonObject {
  def api[T](update: Series => T) = new SeriesAPI(this, update)
}

class SeriesAPI[T](series: Series, update: Series => T) extends API {
  def name(s: String) = update(series.copy(name = s))

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(series.copy(other = series.other + (name ->
    value)))

}


sealed trait Point extends CustomJsonObject

case class RichPoint(x: Option[Double] = None,
                     y: Option[Double] = None,
                     color: Color = null,
                     dataLabels: DataLabels = null,
                     name: String = null,
                     other: Map[String, JsValue] = Map()) extends Point

case class XYValue(x: Double, y: Double,
                   other: Map[String, JsValue] = Map()) extends Point

case class YValue(value: Double,
                  other: Map[String, JsValue] = Map()) extends Point

case class DataLabels(x: Option[Int] = None,
                      y: Option[Int] = None,
                      other: Map[String, JsValue] = Map()) extends CustomJsonObject



