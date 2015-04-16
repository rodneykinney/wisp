package allenai.highcharts

import com.quantifind.charts.Plotter
import com.quantifind.charts.repl.{XYData, IterablePair}

/**
 * Created by rodneykinney on 4/14/15.
 */
trait BasePlot[T <: BasePlot[T]] {
  def api: HighchartAPI = data
  def api_=(h: HighchartAPI) = update(h)
  protected var data: HighchartAPI
  val plotter: Plotter[HighchartAPI, HighchartAPI]

  plotter.addPlot(data)

  def update(newData: HighchartAPI): T = {
    data = plotter.updatePlot(data, newData)
    this.asInstanceOf[T]
  }

  def title = api.title.wrapper(t => update(data.copy(title = t)))

  def addSeries(xyData: XYData) =
    update(api.copy(series = api.series :+ (Series.pairs(data = xyData.xy, `type` = api.series.head.`type`))))

  def layout = api.chart.wrapper(c => update(data.copy(chart = c)))

  def exporting = api.exporting.wrapper(e => update(data.copy(exporting = e)))
}

class LinePlot(var data: HighchartAPI, val plotter: Plotter[HighchartAPI, HighchartAPI])
    extends BasePlot[LinePlot] with HasLegend[LinePlot] with HasXYAxis[LinePlot]

trait HasXYAxis[T <: BasePlot[T]] extends BasePlot[T] {

  def xAxis(idx: Int): AxisAPI[T] = {
    val axis: Axis = api.xAxis(idx)
    axis.wrapper { a =>
      update(data.copy(xAxis = data.xAxis.updated(idx,a)))
    }
  }
  def xAxis: AxisAPI[T] = xAxis(0)
  def yAxis(idx: Int): AxisAPI[T] = {
    val axis: Axis = api.yAxis(idx)
    axis.wrapper { a =>
      update(data.copy(yAxis = data.yAxis.updated(idx,a)))
    }
  }
  def yAxis: AxisAPI[T] = yAxis(0)
  def addXAxis(axis: Axis = Axis()) = update(data.copy(xAxis = data.xAxis :+ axis))
  def addYAxis(axis: Axis = Axis()) = update(data.copy(yAxis = data.yAxis :+ axis))
}

trait HasLegend[T <: BasePlot[T]] extends BasePlot[T] {
  // Assign names to series, if mis-matched lengths use the shorter one as a cut-off
  def legend(labels: String*) = {
    val labelArray = labels.toArray
    val newSeries = data.series.toSeq.zipWithIndex.map { case (s, idx) => if (idx >= labels.size) s else s.copy(name = labelArray(idx)) }
    update(data.copy(series = newSeries))
  }
}

trait HasStacking[T <: BasePlot[T]] extends BasePlot[T] {
  // Combines points with the same x-value into a single visualization point
  // normal stacking adds the values in order of the corresponding series
  // percentage stacking creates a distribution from the values
  //  def stack(stackType: Stacking.Type) = {
  //    update(data.copy(plotOptions = PlotOptions(series = PlotOptionKey(stacking = stackType))))
  //  }

  // Undoes the effect of calling stack()
  //  def unstack() = {
  //    update(data.copy(plotOptions = PlotOptions(series = PlotOptionKey(stacking = None))))
  //  }
}
