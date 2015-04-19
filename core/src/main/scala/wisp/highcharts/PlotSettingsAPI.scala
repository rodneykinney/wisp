package wisp.highcharts

import spray.json.{JsonWriter, JsValue}
import wisp.CustomJsonObject

import java.awt.Color
import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
case class PlotSpecificSettings(
                        area: SeriesSettings = null,
                        areaspline: SeriesSettings = null,
                        bar: SeriesSettings = null,
                        column: SeriesSettings = null,
                        line: SeriesSettings = null,
                        series: SeriesSettings = null,
                        other: Map[String, JsValue] = Map()) extends CustomJsonObject

case class SeriesSettings(
                           stacking: Stacking = null,
                           shadow: Option[Boolean] = None,
                           color: Color = null,
                           dashStyle: DashStyle = null,
                           lineWidth: Option[Int] = None,
                           marker: MarkerConfig = null,
                           other: Map[String, JsValue] = Map()) extends CustomJsonObject {
  def api[T](update: SeriesSettings => T) = new SeriesSettingsAPI[T](this, update)
}

class SeriesSettingsAPI[T](s: SeriesSettings, update: SeriesSettings => T) extends API {
  @WebMethod
  def dashStyle(x: DashStyle) = update(s.copy(dashStyle = x))

  @WebMethod
  def color(x: Color) = update(s.copy(color = x))

  @WebMethod
  def lineWidth(x: Int) = update(s.copy(lineWidth = Some(x)))

  @WebMethod
  def marker = Option(s.marker).getOrElse(MarkerConfig()).api(m => update(s.copy(marker = m)))

  @WebMethod
  def shadow(x: Boolean) = update(s.copy(shadow = Some(x)))

  @WebMethod
  def stacked = stacking(Stacking.normal)

  @WebMethod
  def stacking(x: Stacking) = update(s.copy(stacking = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def addOption[V: JsonWriter](name: String, value: V)
  = update(s.copy(other = s.other + (name -> implicitly[JsonWriter[V]].write(value))))

}

case class MarkerConfig(
                         enabled: Option[Boolean] = None,
                         fillColor: Color = null,
                         symbol: MarkerSymbol = null,
                         other: Map[String, JsValue] = Map()) extends CustomJsonObject {
  def api[T](update: MarkerConfig => T) = new MarkerAPI[T](this, update)
}

class MarkerAPI[T](m: MarkerConfig, update: MarkerConfig => T) extends API {
  @WebMethod
  def enabled(x: Boolean) = update(m.copy(enabled = Some(x)))

  @WebMethod
  def color(x: Color) = update(m.copy(fillColor = x))

  @WebMethod
  def symbol(x: MarkerSymbol) = update(m.copy(symbol = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def addOption[V: JsonWriter](name: String, value: V)
  = update(m.copy(other = m.other + (name -> implicitly[JsonWriter[V]].write(value))))

}

