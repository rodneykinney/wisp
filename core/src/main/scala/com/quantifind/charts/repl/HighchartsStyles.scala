package com.quantifind.charts.repl

import com.quantifind.charts.highcharts._

/**
 * User: austin
 * Date: 3/25/15
 */
/**
 * Defines auxiliary tools available to plots, such as adding a Title
 */
trait HighchartsStyles extends Hold with Labels[Highchart] with WebPlotHighcharts {
  import Highchart._

  override def plot(t: Highchart): Highchart = {
    val newPlot =
      if (isHeld && plots.nonEmpty) {
        val oldplot = plots.head
        plots = plots.tail
        // Throws away things from t besides the series!
        oldplot.copy(series = oldplot.series ++ t.series)
      } else t
    super.plot(newPlot)
  }

  // Used in conjunction with IterablePair type-class for plotting
  def xyToSeries[T1, T2](x: Iterable[T1], y: Iterable[T2], chartType: SeriesType.Type) =
    Highchart(Series(x.zip(y).toSeq, chart = chartType))

  def updatePlot(f: (Highchart => Highchart)): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = f(plot)
    super.plot(newPlot)
  }

  def updatePlot[A](f: ((Highchart, A) => Highchart), s: A): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = f(plot, s)
    super.plot(newPlot)
  }

  // Assigns a label to the axis
  @deprecated
  def xAxis(plot: Highchart, label: String): Highchart = {
    plot.xAxis(label)
  }
  @deprecated
  def xAxis(label: String): Highchart = {
    updatePlot(xAxis _, label)
  }
  @deprecated
  def yAxis(plot: Highchart, label: String): Highchart = {
    plot.yAxis(label)
  }
  @deprecated
  def yAxis(label: String): Highchart = {
    updatePlot(yAxis _, label)
  }

  // Change the type of the x-axis, ie logarithmic
  // To convert to categories prefer xAxisCategories
  @deprecated
  def xAxisType(plot: Highchart, label: AxisType.Type): Highchart = {
    plot.xAxisType(label)
  }
  @deprecated
  def xAxisType(label: AxisType.Type): Highchart = {
    updatePlot(xAxisType _, label)
  }
  @deprecated
  def yAxisType(plot: Highchart, label: AxisType.Type): Highchart = {
    plot.yAxisType(label)
  }
  @deprecated
  def yAxisType(label: AxisType.Type): Highchart = {
    updatePlot(yAxisType _, label)
  }

  // Modifies the x-axis to use String based category names instead of a numeric series
  def xAxisCategories(plot: Highchart, categories: Iterable[String]): Highchart = {
    plot.xAxisCategories(categories)
  }
  def xAxisCategories(categories: Iterable[String]): Highchart = {
    updatePlot(xAxisCategories _, categories)
  }
  def xAxisCategories(categories: String*): Highchart = {
    updatePlot(xAxisCategories _, List(categories: _*))
  }
  def yAxisCategories(plot: Highchart, categories: Iterable[String]): Highchart = {
    plot.yAxisCategories(categories)
  }
  def yAxisCategories(categories: Iterable[String]): Highchart = {
    updatePlot(yAxisCategories _, categories)
  }
  def yAxisCategories(categories: String*): Highchart = {
    updatePlot(xAxisCategories _, List(categories: _*))
  }

  // Changes the title at the top of the chart
  def title(plot: Highchart, label: String): Highchart = {
    plot.title(label)
  }
  def title(label: String): Highchart = {
    updatePlot(title _, label)
  }

  // Assign names to series, if mis-matched lengths use the shorter one as a cut-off
  def legend(plot: Highchart, labels: Iterable[String]): Highchart = {
    plot.legend(labels)
  }
  def legend(labels: Iterable[String]): Highchart = {
    updatePlot(legend _, labels)
  }
  def legend(labels: String*): Highchart = {
    updatePlot(legend _, List(labels: _*))
  }

  // Combines points with the ame x-value into a single visualization point
  // normal stacking adds the values in order of the corresponding series
  // percentage stacking creates a distribution from the values
  def stack(plot: Highchart, stackType: Stacking.Type): Highchart = {
    plot.stack(stackType)
  }
  def stack(stackType: Stacking.Type = Stacking.NORMAL): Highchart = {
    updatePlot(stack _, stackType)
  }

  // removes stacking
  def unstack(plot: Highchart): Highchart = {
    plot.unstack()
  }
  def unstack(): Highchart = {
    updatePlot(unstack _)
  }
}
