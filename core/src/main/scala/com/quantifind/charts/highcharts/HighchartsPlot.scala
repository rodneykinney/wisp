package com.quantifind.charts.highcharts

import allenai.Plot
import com.quantifind.charts.Plotter
import com.quantifind.charts.repl.IterablePair

import scala.collection.{Seq, Iterable}
import allenai.Plot._

/**
 * Created by rodneykinney on 4/14/15.
 */
trait HPlot[T <: HPlot[T]] {
  var data: Highchart
  def plotter: Plotter[Highchart]

  plotter.addPlot(data)

  protected def update(d: Highchart): T = {
    data = d
    plotter.updatePlot(d)
    this.asInstanceOf[T]
  }

  def title(s: String) = update(data.copy(title = Some(Title(s))))

  def addSeries[A, B](xy: IterablePair[A, B]) =
    update(data.copy(series = data.series ++ Traversable(Series(xy, data.series.head.chart.get))))
}

class LinePlot(var data: Highchart, val plotter: Plotter[Highchart])
    extends HPlot[LinePlot] with HasLegend[LinePlot] with HasXYAxis[LinePlot]

trait HasXYAxis[T <: HPlot[T]] extends HPlot[T] {
  // Axis
  implicit def axisTitleOptionToArrayAxes(axisTitle: Option[AxisTitle]) = Some(Array(Axis(axisTitle)))

  implicit def axisToArrayAxes(axis: Axis) = Some(Array(axis))

  implicit def axesSeqToSomeAxesArray(axes: Seq[Axis]) = Some(axes.toArray)

  implicit def stringToAxisTitle(s: String) = Some(AxisTitle(s))

  implicit def stringToAxis(s: String): Option[Array[Axis]] = axisTitleOptionToArrayAxes(stringToAxisTitle(s))

  // Axis Labels
  def xAxisLabel(label: String) = {
    update(data.copy(xAxis = Some(data.xAxis match {
      case Some(axisArray) if axisArray.size > 0 => axisArray.map {
        _.copy(title = label)
      }
      case _ => Array(Axis(AxisTitle(label)))
    })))
  }

  def yAxisLabel(label: String) = {
    update(data.copy(yAxis = Some(data.yAxis match {
      case Some(axisArray) if axisArray.size > 0 => axisArray.map {
        _.copy(title = label)
      }
      case _ => Array(Axis(AxisTitle(label)))
    })))
  }

  // Change the type of the axis, ie logarithmic
  // To convert to categories prefer xAxisCategories
  def xAxisType(axisType: AxisType) = {
    if (!AxisType.values.contains(axisType)) {
      println(s"Not an acceptable axis type. Options are: ${AxisType.values.mkString(", ")}.")
      this.asInstanceOf[T]
    }
    update(data.copy(xAxis = Some(data.xAxis match {
      case Some(axisArray) if axisArray.size > 0 => axisArray.map {
        _.copy(axisType = axisType)
      }
      case _ => Array(Axis(axisType = axisType))
    })))
  }

  def yAxisType(axisType: AxisType) = {
    update(data.copy(yAxis = data.yAxis match {
      case Some(axisArray) if axisArray.size > 0 => axisArray.map {
        _.copy(axisType = axisType)
      }
      case _ => Array(Axis(axisType = axisType))
    }))
  }

  // Modifies the axis to use String based category names instead of a numeric series
  def xAxisCategories(categories: Iterable[String]) = {
    update(data.copy(xAxis = modifyAxis(data.xAxis)(
      _.copy(
        axisType = AxisType.category,
        categories = Some(categories.toArray)
      ))))
  }

  def yAxisCategories(categories: Iterable[String]) = {
    update(data.copy(yAxis = modifyAxis(data.yAxis)
        (_.copy(
          axisType = AxisType.category,
          categories = Some(categories.toArray)
        ))))
  }

  private def modifyAxis(input: Option[Array[Axis]])(copier: Axis => Axis): Option[Array[Axis]] = {
    val copied: Array[Axis] = input match {
      case Some(axisArray) if axisArray.size > 0 => axisArray.map(copier)
      case _ => Array(copier(Axis()))
    }
    Some(copied)

  }
  def xAxisRange(min: Number, max: Number) = {
    update(data.copy(xAxis = modifyAxis(data.xAxis)(
      _.copy(min = min, max = max))))
  }
  def yAxisRange(min: Number, max: Number) = {
    update(data.copy(yAxis = modifyAxis(data.yAxis)(
      _.copy(min = min, max = max))))

  }
}

trait HasLegend[T <: HPlot[T]] extends HPlot[T] {
  // Assign names to series, if mis-matched lengths use the shorter one as a cut-off
  def legend(labels: String*) = {
    val labelArray = labels.toArray
    val newSeries = data.series.toSeq.zipWithIndex.map { case (s, idx) => if (idx >= labels.size) s else s.copy(name = Some(labelArray(idx))) }
    update(data.copy(series = newSeries))
  }
}

trait HasStacking[T <: HPlot[T]] extends HPlot[T] {
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
