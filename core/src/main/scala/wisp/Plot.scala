package wisp

import wisp.highcharts._

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends SeriesDataConversions {
  implicit val plotter = new HighchartsWebPlotter

  private def hc(data: SeriesData, st: SeriesType) =
    HighchartAPI(series = Vector(Series(data.points, `type` = st)))

  def line(data: SeriesData) = new LinePlot(hc(data, SeriesType.line), plotter)

  def area(data: SeriesData) = new LinePlot(hc(data, SeriesType.area), plotter)

  def areaSpline(data: SeriesData) = new LinePlot(hc(data, SeriesType.areaspline), plotter)

  def bar(data: SeriesData) = new BarPlot(hc(data, SeriesType.bar), plotter)

  def column(data: SeriesData) = new ColumnPlot(hc(data, SeriesType.column), plotter)
}

