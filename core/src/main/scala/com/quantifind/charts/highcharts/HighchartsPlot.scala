package com.quantifind.charts.highcharts

import allenai.Plot
import com.quantifind.charts.Plotter
import com.quantifind.charts.repl.IterablePair

import scala.collection.Iterable
import allenai.Plot._

/**
 * Created by rodneykinney on 4/14/15.
 */
class HighchartsPlot(
    val data: Highchart)(
    implicit plotter: Plotter[Highchart]) {

  def title(s: String) = update(data.copy(title = Some(Title(s))))

  def addSeries[A, B](xy: IterablePair[A, B]) =
    update(data.copy(series = data.series ++ Traversable(Series(xy, data.series.head.chart.get))))

  // Assign names to series, if mis-matched lengths use the shorter one as a cut-off
  def legend(labels: String*) = {
    val labelArray = labels.toArray
    val newSeries = data.series.toSeq.zipWithIndex.map { case (s, idx) => if (idx >= labels.size) s else s.copy(name = Some(labelArray(idx))) }
    update(data.copy(series = newSeries))
  }

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



  private def update(d: Highchart) = {
    plotter.updatePlot(d)
    new HighchartsPlot(d)
  }
}
