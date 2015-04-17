package allenai

import allenai.highcharts._
import com.quantifind.charts.WebPlotter
import com.quantifind.charts.repl._

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends WebPlotter with IterablePairConversions {
  implicit val plotter = this

  private def hc(data: SeriesData, st: SeriesType) =
    HighchartAPI(series = Vector(Series(data.points, `type` = st)))

  def line(data: SeriesData) = new LinePlot(hc(data, SeriesType.line), plotter)

  def area(data: SeriesData) = new LinePlot(hc(data, SeriesType.area), plotter)

  def areaSpline(data: SeriesData) = new LinePlot(hc(data, SeriesType.areaspline), plotter)

  def bar(data: SeriesData) = new BarPlot(hc(data, SeriesType.bar), plotter)

  def column(data: SeriesData) = new ColumnPlot(hc(data, SeriesType.column), plotter)
}


