package com.quantifind.charts.highcharts

import allenai.Plot
import com.quantifind.charts.Plotter
import com.quantifind.charts.repl.IterablePair

import scala.collection.{Seq, Iterable}
import allenai.Plot._

/**
 * Created by rodneykinney on 4/14/15.
 */
trait BasePlot[T <: BasePlot[T]] {
  protected var data: Highchart
  val plotter: Plotter[Highchart, Highchart]

  plotter.addPlot(data)

  protected def update(newData: Highchart): T = {
    data = plotter.updatePlot(data, newData)
    this.asInstanceOf[T]
  }

  def title(s: String) = update(data.copy(title = Some(Title(s))))

  def addSeries[A, B](xy: IterablePair[A, B]) =
    update(data.copy(series = data.series ++ Traversable(Series(xy, data.series.head.chart.get))))
}

class LinePlot(var data: Highchart, val plotter: Plotter[Highchart, Highchart])
    extends BasePlot[LinePlot] with HasLegend[LinePlot] with HasXYAxis[LinePlot]

trait HasXYAxis[T <: BasePlot[T]] extends BasePlot[T] {

  // Axis Labels
  def xAxisLabel(label: String) = {
    update(data.updateXAxis(_.copy(title = Some(AxisTitle(label)))))
  }

  def yAxisLabel(label: String) = {
    update(data.updateYAxis(_.copy(title = Some(AxisTitle(label)))))
  }

  // Change the type of the axis, ie logarithmic
  def xAxisType(axisType: AxisType) = {
    update(data.updateXAxis(_.copy(axisType = axisType.toString)))
  }

  def yAxisType(axisType: AxisType) = {
    update(data.updateYAxis(_.copy(axisType = axisType.toString)))
  }

  def xAxisRange(min: Number, max: Number) = {
    update(data.updateXAxis(_.copy(min = min, max = max)))
  }
  def yAxisRange(min: Number, max: Number) = {
    update(data.updateYAxis(_.copy(min = min, max = max)))

  }
}

trait HasLegend[T <: BasePlot[T]] extends BasePlot[T] {
  // Assign names to series, if mis-matched lengths use the shorter one as a cut-off
  def legend(labels: String*) = {
    val labelArray = labels.toArray
    val newSeries = data.series.toSeq.zipWithIndex.map { case (s, idx) => if (idx >= labels.size) s else s.copy(name = Some(labelArray(idx))) }
    update(data.copy(series = newSeries))
  }
}

trait HasStacking[T <: BasePlot[T]] extends BasePlot[T] {
  // Combines points with the same x-value into a single visualization point
  // normal stacking adds the values in order of the corresponding series
  // percentage stacking creates a distribution from the values
  def stack(stackType: Stacking.Type) = {
    update(data.copy(plotOptions = PlotOptions(series = PlotOptionKey(stacking = stackType))))
  }

  // Undoes the effect of calling stack()
  def unstack() = {
    update(data.copy(plotOptions = PlotOptions(series = PlotOptionKey(stacking = None))))
  }
}
