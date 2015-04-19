package wisp.highcharts

import spray.json.{JsonWriter, JsValue}
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
  @WebMethod
  def name(s: String) = update(series.copy(name = s))

  @WebMethod
  def seriesType(t: SeriesType)= update(series.copy(`type` = t))

  @WebMethod
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
          case r: RichPoint =>
            r.copy(dataLabels = label)
          case _ =>
            RichPoint(x = p.X, y = p.Y, dataLabels = label)
        }
      }
    update(series.copy(data = newData))
  }

  @WebMethod(action = "Add additional values to the JSON object")
  def addOption[V: JsonWriter](name: String, value: V)
  = update(series.copy(other = series.other + (name -> implicitly[JsonWriter[V]].write(value))))

}

sealed trait Point {
  def X: Option[Double]
  def Y: Option[Double]
  def Name: Option[String]
}

case class RichPoint(name: String = null,
                     x: Option[Double] = None,
                     y: Option[Double] = None,
                     color: Color = null,
                     dataLabels: DataLabel = null,
                     other: Map[String, JsValue] = Map()) extends Point with CustomJsonObject {
  def X = x
  def Y = y
  def Name = Some(name)
}

case class XYValue(x: Double, y: Double) extends Point {
  def X = Some(x)
  def Y = Some(y)
  def Name = None
}

case class YValue(value: Double) extends Point {
  def X = Some(value)
  def Y = None
  def Name = None
}

case class DataLabel(backgroundColor: Color = null,
                     color: Color = null,
                     style: Map[String, String] = null,
                     verticalAlign: VAlign = null,
                     x: Option[Int] = None,
                     y: Option[Int] = None,
                     align: HAlign = null,
                     enabled: Boolean = true,
                     other: Map[String, JsValue] = Map()) extends CustomJsonObject



