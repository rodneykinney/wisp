package wisp.highcharts

import wisp.Plotter

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

  def remove() = plotter.removePlot(data)

  def title = api.title.update(t => update(data.copy(title = t)))

  def addSeries(xyData: SeriesData) =
    update(api.copy(series = api.series :+ (Series(data = xyData.points, `type` = api.series.head.`type`))))

  def series(idx: Int) = data.series(idx).update(s => update(data.copy(series = data.series.updated(idx, s))))

  def layout = api.chart.update(c => update(data.copy(chart = c)))

  def exporting = api.exporting.update(e => update(data.copy(exporting = e)))
}

class LinePlot(var data: HighchartAPI, val plotter: Plotter[HighchartAPI, HighchartAPI])
    extends BasePlot[LinePlot] with HasLegend[LinePlot] with HasXYAxis[LinePlot]

class BarPlot(var data: HighchartAPI, val plotter: Plotter[HighchartAPI, HighchartAPI])
    extends BasePlot[BarPlot] with HasLegend[BarPlot] with HasXYAxis[BarPlot] with HasStacking[BarPlot] {
  override def updateOptions(o: PlotOptions, f: (PlotSettings) => PlotSettings): PlotOptions =
    o.copy(bar = f(o.bar))
}

class ColumnPlot(var data: HighchartAPI, val plotter: Plotter[HighchartAPI, HighchartAPI])
    extends BasePlot[ColumnPlot] with HasLegend[ColumnPlot] with HasXYAxis[ColumnPlot] with HasStacking[ColumnPlot] {
  override def updateOptions(o: PlotOptions, f: (PlotSettings) => PlotSettings): PlotOptions =
    o.copy(column = f(o.column))
}

trait HasXYAxis[T <: BasePlot[T]] extends BasePlot[T] {

  def xAxis(idx: Int): AxisAPI[T] = {
    val axis: Axis = api.xAxis(idx)
    axis.update { a =>
      update(data.copy(xAxis = data.xAxis.updated(idx, a)))
    }
  }
  def xAxis: AxisAPI[T] = xAxis(0)
  def yAxis(idx: Int): AxisAPI[T] = {
    val axis: Axis = api.yAxis(idx)
    axis.update { a =>
      update(data.copy(yAxis = data.yAxis.updated(idx, a)))
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
    val newSeries = data.series.zipWithIndex.map { case (s, idx) => if (idx >= labels.size) s else s.copy(name = labelArray(idx)) }
    update(data.copy(series = newSeries))
  }
}

trait HasStacking[T <: BasePlot[T]] extends BasePlot[T] {
  def updateOptions(o: PlotOptions, f: PlotSettings => PlotSettings): PlotOptions
  def stack(): T = stack(Stacking.normal)
  def stack(s: Stacking): T = update(
    data.copy(plotOptions = updateOptions(
      Option(data.plotOptions).getOrElse(PlotOptions()),
      settings => Option(settings).getOrElse(PlotSettings()).copy(stacking = s)))
  )
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
