package wisp.highcharts

import wisp.Plotter

/**
 * Created by rodneykinney on 4/14/15.
 */
trait Plot {
var data: Highchart
}

trait BasePlot[T <: BasePlot[T]] extends Plot {
  def api = data.api(update)

  val plotter: Plotter[Plot, Int]

  val index = plotter.addPlot(this)

  def update(newData: Highchart): T = {
    data = newData
    plotter.updatePlot(index, this)
    this.asInstanceOf[T]
  }

  def remove() = plotter.removePlot(index)

  def title = api.title
  def addSeries(xyData: SeriesData) = api.addSeries(xyData)
  def series(idx: Int) = api.series(idx)
  def layout = api.layout
  def help = api.help
  def exporting = api.exporting
  def colors = api.colors _
  def legend = api.legend
  def addFloatingLabel(x: Int, y: Int, text: String, style: Map[String, String] = Map()) =
    api.addFloatingLabel(x, y, text, style)
}

class LinePlot(var data: Highchart, val plotter: Plotter[Plot, Int])
    extends BasePlot[LinePlot] with HasXYAxis[LinePlot]

class AreaPlot(var data: Highchart, val plotter: Plotter[Plot, Int])
    extends BasePlot[AreaPlot] with HasXYAxis[AreaPlot] with HasStacking[AreaPlot] {
  override def updateOptions(o: PlotOptions, f: (PlotSettings) => PlotSettings): PlotOptions =
    o.copy(area = f(o.area))
}

class BarPlot(var data: Highchart, val plotter: Plotter[Plot, Int])
    extends BasePlot[BarPlot] with HasXYAxis[BarPlot] with HasStacking[BarPlot] {
  override def updateOptions(o: PlotOptions, f: (PlotSettings) => PlotSettings): PlotOptions =
    o.copy(bar = f(o.bar))
}

class ColumnPlot(var data: Highchart, val plotter: Plotter[Plot, Int])
    extends BasePlot[ColumnPlot] with HasXYAxis[ColumnPlot] with HasStacking[ColumnPlot] {
  override def updateOptions(o: PlotOptions, f: (PlotSettings) => PlotSettings): PlotOptions =
    o.copy(column = f(o.column))
}

trait HasXYAxis[T <: BasePlot[T]] extends BasePlot[T] {

  def xAxis(idx: Int): AxisAPI[T] = {
    val axis: Axis = data.xAxis(idx)
    axis.update { a =>
      update(data.copy(xAxis = data.xAxis.updated(idx, a)))
    }
  }
  def xAxis: AxisAPI[T] = xAxis(0)
  def yAxis(idx: Int): AxisAPI[T] = {
    val axis: Axis = data.yAxis(idx)
    axis.update { a =>
      update(data.copy(yAxis = data.yAxis.updated(idx, a)))
    }
  }
  def yAxis: AxisAPI[T] = yAxis(0)
  def addXAxis(axis: Axis = Axis()) = update(data.copy(xAxis = data.xAxis :+ axis))
  def addYAxis(axis: Axis = Axis()) = update(data.copy(yAxis = data.yAxis :+ axis))
}

trait HasStacking[T <: BasePlot[T]] extends BasePlot[T] {
  def updateOptions(o: PlotOptions, f: PlotSettings => PlotSettings): PlotOptions
  def stack(): T = stack(Stacking.normal)
  def stack(s: Stacking): T = update(
    data.copy(plotOptions = updateOptions(
      Option(data.plotOptions).getOrElse(PlotOptions()),
      settings => Option(settings).getOrElse(PlotSettings()).copy(stacking = s)))
  )
}
