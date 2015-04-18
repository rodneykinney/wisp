package wisp

import wisp.highcharts._

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends HighchartsHtmlPlotter with SeriesDataConversions {
  implicit val plotter = this

  private def hc(data: SeriesData, st: SeriesType) =
    RootConfig(series = Vector(Series(data.points, `type` = st)))

  def line(data: SeriesData) = new RootAPI(hc(data, SeriesType.line), plotter)

  def area(data: SeriesData) = new RootAPI(hc(data, SeriesType.area), plotter)

  def areaSpline(data: SeriesData) = new RootAPI(hc(data, SeriesType.areaspline), plotter)

  def bar(data: SeriesData) = new RootAPI(hc(data, SeriesType.bar), plotter)

  def column(data: SeriesData) = new RootAPI(hc(data, SeriesType.column), plotter)
}


