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
    val hc = data match {
      case xy: XYData =>
        HighchartAPI.pairs(data = xy.xy, `type` = SeriesType.line)
      case xy: LabeledXData =>
        val (x, labels) = xy.x.unzip
        val h = HighchartAPI.singleValue(x, SeriesType.line)
        h.copy(yAxis = Vector(Axis(`type` = AxisType.category, categories = labels.toIndexedSeq)))
      case xy: LabeledYData =>
        val (labels, y) = xy.y.unzip
        val h = HighchartAPI.singleValue(data = y, SeriesType.line)
        h.copy(xAxis = Vector(Axis(`type` = AxisType.category, categories = labels.toIndexedSeq)))
      case xy: LabeledXYData =>
        val (xLabels, yLabels) = xy.xyLabels.unzip
        val h = HighchartAPI.pairs(data = (0 until xy.xyLabels.size).map(t => (t, t)), `type` = SeriesType.line)
        h.copy(
          xAxis = Vector(Axis(`type` = AxisType.category,
            categories = xLabels.toIndexedSeq)),
          yAxis = Vector(Axis(`type` = AxisType.category,
            categories = yLabels.toIndexedSeq)))
    }
    new LinePlot(hc, plotter)
  }
}


