package wisp

import wisp.highcharts._

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends HighchartsHtmlDisplay with SeriesDataConversions {
  implicit val display = this

  private def api(data: SeriesData,
                  st: SeriesType) = {
    val config = RootConfig(series = Vector(Series(data.points, `type` = st)))
    val base = new GenericChartAPI(config, display)
    data.categories match {
      case Some(c) => base.xAxis.categories(c)
      case None => base
    }
  }

  def line(data: SeriesData) = api(data, SeriesType.line)

  def area(data: SeriesData) = api(data, SeriesType.area)

  def areaSpline(data: SeriesData) = api(data, SeriesType.areaspline)

  def bar(data: SeriesData) = api(data, SeriesType.bar)

  def column(data: SeriesData) = api(data, SeriesType.column)

  def histogram(data: SeriesData, numBins: Int = 50) = {
    val config = RootConfig(series = Vector(Series(data.points, `type` = SeriesType.column)))
    new HistogramAPI(config, display, numBins)
  }
}


