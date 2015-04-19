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

  def settings = SeriesSettings().api {
    import HighchartsJson._
    import spray.json._
    newSettings =>
      var o = this.series.other
      for ((name, value) <- newSettings.toJson.asInstanceOf[JsObject].fields) {
        o += (name -> value)
      }
      update(this.series.copy(other = o))
  }

  @WebMethod(action = "Draw labels next to individual points in the series")
  def showPointLabels(labeler: Int => DataLabel = i => DataLabel()) = {
    val newData =
      for ((p, i) <- series.data.zipWithIndex) yield {
        val label = labeler(i)
        p match {
          case XYValue(x, y) =>
            RichPoint(x = Some(x), y = Some(y), dataLabels = label)
          case YValue(y) =>
            RichPoint(x = None, y = Some(y), dataLabels = label)
          case r: RichPoint =>
            r.copy(dataLabels = label)
        }
      }
    update(series.copy(data = newData))
  }

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(series.copy(other = series.other + (name ->
    value)))

}

sealed trait Point

case class RichPoint(x: Option[Double] = None,
                     y: Option[Double] = None,
                     color: Color = null,
                     dataLabels: DataLabel = null,
                     name: String = null,
                     other: Map[String, JsValue] = Map()) extends Point with CustomJsonObject

case class XYValue(x: Double, y: Double) extends Point

case class YValue(value: Double) extends Point

case class DataLabel(backgroundColor: Color = null,
                     color: Color = null,
                     style: Map[String, String] = null,
                     verticalAlign: VAlign = null,
                     x: Option[Int] = None,
                     y: Option[Int] = None,
                     align: HAlign = null,
                     enabled: Boolean = true,
                     other: Map[String, JsValue] = Map()) extends CustomJsonObject



