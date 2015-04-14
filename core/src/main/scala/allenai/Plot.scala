package allenai

import com.quantifind.charts.WebPlotter
import com.quantifind.charts.highcharts.{Highchart, HighchartsPlot, SeriesType}
import com.quantifind.charts.repl.{IterablePair, IterablePairConversions}

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends WebPlotter with IterablePairConversions with ContainerConversions {
  implicit val plotter = this
  def line[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = Highchart(xr, yr, SeriesType.line)
    plotter.addPlot(hc)
    new HighchartsPlot(hc)
  }
}

trait ContainerConversions {
  // value -> Some(value)
  implicit def optionWrap[T](value: T): Option[T] = Option(value)

}
