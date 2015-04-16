package allenai

import allenai.highcharts._
import com.quantifind.charts.WebPlotter
import com.quantifind.charts.repl._

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends WebPlotter with IterablePairConversions {
  implicit val plotter = this

  def line(data: SeriesData) = {
    val hc = HighchartAPI(series = List(Series(data.points, `type` = SeriesType.line)))
    new LinePlot(hc, plotter)
  }

  def area(data: SeriesData) =
    new LinePlot(HighchartAPI(series = List(Series(data.points, `type` = SeriesType.area))), plotter)
}


