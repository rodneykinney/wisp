package wisp.highcharts

import spray.json.JsValue
import wisp.CustomJsonObject

/**
 * Created by rodneykinney on 4/18/15.
 */
case class PlotOptions(
                        area: PlotSettings = null,
                        areaspline: PlotSettings = null,
                        bar: PlotSettings = null,
                        column: PlotSettings = null,
                        line: PlotSettings = null,
                        series: PlotSettings = null,
                        other: Map[String, JsValue] = Map()) extends CustomJsonObject

case class PlotSettings(
                         stacking: Stacking = null,
                         shadow: Option[Boolean] = None,
                         other: Map[String, JsValue] = Map()) extends CustomJsonObject

