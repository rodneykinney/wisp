package wisp

import wisp.highcharts._

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends HighchartsHtmlPlotter with SeriesDataConversions {
  implicit val plotter = this

  private def api(data: SeriesData,
                  st: SeriesType)(
                   buildAPI: RootConfig => RootAPI = c => new RootAPI(c, plotter)) = {
    val base = buildAPI(RootConfig(series = Vector(Series(data.points, `type` = st))))
    data.categories match {
      case Some(c) => base.xAxis.categories(c)
      case None => base
    }
  }

  def line(data: SeriesData) = api(data, SeriesType.line)()

  def area(data: SeriesData) = api(data, SeriesType.area)()

  def areaSpline(data: SeriesData) = api(data, SeriesType.areaspline)()

  def bar(data: SeriesData) = api(data, SeriesType.bar)()

  def column(data: SeriesData) = api(data, SeriesType.column)()

  def histogram(data: SeriesData, numBins: Int = 50) = api(data, SeriesType.column) {
    c => new HistogramAPI(c, plotter, numBins)
  }
}


