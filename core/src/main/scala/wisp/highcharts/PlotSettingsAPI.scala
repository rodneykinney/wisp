package wisp.highcharts

import spray.json.JsValue
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
  def dashStyle(x: DashStyle) = update(s.copy(dashStyle = x))

  def color(x: Color) = update(s.copy(color = x))

  def lineWidth(x: Int) = update(s.copy(lineWidth = Some(x)))

  def marker = Option(s.marker).getOrElse(MarkerConfig()).api(m => update(s.copy(marker = m)))

  def shadow(x: Boolean) = update(s.copy(shadow = Some(x)))

  def stacked = stacking(Stacking.normal)

  def stacking(x: Stacking) = update(s.copy(stacking = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(s.copy(other = s.other + (name -> value)))

}

case class MarkerConfig(
                         enabled: Option[Boolean] = None,
                         fillColor: Color = null,
                         symbol: MarkerSymbol = null,
                         other: Map[String, JsValue] = Map()) extends CustomJsonObject {
  def api[T](update: MarkerConfig => T) = new MarkerAPI[T](this, update)
}

class MarkerAPI[T](m: MarkerConfig, update: MarkerConfig => T) extends API {
  def enabled(x: Boolean) = update(m.copy(enabled = Some(x)))

  def color(x: Color) = update(m.copy(fillColor = x))

  def symbol(x: MarkerSymbol) = update(m.copy(symbol = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(m.copy(other = m.other + (name -> value)))

}

