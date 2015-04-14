package com.quantifind.charts

import com.quantifind.charts.highcharts._
import com.quantifind.charts.repl._
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

/**
 * User: austin
 * Date: 12/2/14
 *
 * Highcharts implementation of plotting functionality. Includes several highcharts specific plots
 *
 */

object Highcharts extends BinnedDataLowerPriorityImplicits with IterablePairConversions with HighchartsStyles {

  implicit def binIterableNumBins[A: Numeric](data: Iterable[A], numBins: Int): BinnedData = new
      IterableBinned[A](data, numBins)

  implicit def mkPair[A, B: Numeric](data: Iterable[(A, B)]) = new PairBinned(data)

  implicit def mkTrueTriplet[A, B, C: Numeric](data: Iterable[(A, B, C)]) = new TrueTripletBinned(data)

  implicit def mkCoupledTriplet[A, B, C: Numeric](data: Iterable[((A, B), C)]) = new
      CoupledTripletBinned(data)

  implicit def binIterableNumBins[A: Numeric](data: Array[A], numBins: Int): BinnedData =
    new IterableBinned(data.toSeq, numBins)

  implicit def mkPair[A, B: Numeric](data: Array[(A, B)]) = new PairBinned(data.toSeq)

  implicit def mkTrueTriplet[A, B, C: Numeric](data: Array[(A, B, C)]) =
    new TrueTripletBinned(data.toSeq)

  implicit def mkCoupledTriplet[A, B, C: Numeric](data: Array[((A, B), C)]) =
    new CoupledTripletBinned(data.toSeq)

  // for boxplot
  def invalidSizeBoxPlot() = {
    System.err.println("Warning: tried to create a boxplot from a list that wasn't size 5 - removing invalid elements")
  }

  implicit def mkBoxedDataBoxes[T](data: Iterable[(T, T, T, T, T)]): Iterable[BoxplotData[T]] = {
    data.map { case (low, q1, median, q3, high) => Data(low, q1, median, q3, high)}
  }

  implicit def mkBoxedDataXBoxes[T](data: Iterable[(Any, T, T, T, T, T)]): Iterable[BoxplotData[T]] = {
    data.map { case (x, low, q1, median, q3, high) => Data(x, low, q1, median, q3, high)}
  }

  implicit def mkBoxedDataIterable[T](data: Iterable[Iterable[T]]): Iterable[BoxplotData[T]] = {
    if (data.exists(_.size != 5)) invalidSizeBoxPlot()
    data.filter(_.size == 5).map(_.toList).map { itr => Data(itr(0), itr(1), itr(2), itr(3), itr(4))}
  }

  implicit def mkBoxedDataArray[T](data: Iterable[Array[T]]): Iterable[BoxplotData[T]] = {
    if (data.exists(_.size != 5)) invalidSizeBoxPlot()
    data.filter(_.size == 5).map { itr => Data(itr(0), itr(1), itr(2), itr(3), itr(4))}
  }

  implicit def mkBoxedDataXIterable[T](data: Iterable[(Any, Iterable[T])]): Iterable[BoxplotData[T]] = {
    if (data.exists(_._2.size != 5)) invalidSizeBoxPlot()
    data.filter(_._2.size == 5).map { case (x, itr) => x -> itr.toList}.map { case (x, itr) => Data(x, itr(0), itr(1), itr(2), itr(3), itr(4))}
  }

  implicit def mkBoxedDataXArray[T](data: Iterable[(Any, Array[T])]): Iterable[BoxplotData[T]] = {
    if (data.exists(_._2.size != 5)) invalidSizeBoxPlot()
    data.filter(_._2.size == 5).map { case (x, itr) => Data(x, itr(0), itr(1), itr(2), itr(3), itr(4))}
  }

  def stopServer = stopWispServer

  def startServer() = startWispServer()

  def setPort(port: Int) = setWispPort(port)

  private def addStyle[C, D](hc: Highchart, xy: IterablePair[C, D]) = {
    xy match {
      case s: HasCategories => xAxisCategories(hc, s.getCategories)
      case _ => hc
    }
  }

  def area[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.area)
    plot(addStyle(hc, xy))
  }

  def areaspline[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.areaspline)
    plot(addStyle(hc, xy))
  }

  def bar[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.bar)
    plot(addStyle(hc, xy))
  }

  def boxplot[T](data: Iterable[BoxplotData[T]]) = {
    plot(Highchart(series = Seq(Series(data = data, chart = Some(SeriesType.boxplot)))))
  }

  def column[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.column)
    plot(addStyle(hc, xy))
  }

  def histogram[A: Numeric](data: Iterable[A], numBins: Int) = {
    val binCounts = binIterableNumBins(data, numBins).toBinned().toSeq
    plot(Histogram.histogram(binCounts))
  }

  def histogram(data: BinnedData) = {
    val binCounts = data.toBinned().toSeq
    plot(Histogram.histogram(binCounts))
  }

  def line[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.line)
    plot(addStyle(hc, xy))
  }

  def pie[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.pie)
    plot(addStyle(hc, xy))
  }

  def regression[C: Numeric, D: Numeric](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    LeastSquareRegression.leastSquareRegression(
      xr.toSeq.map(implicitly[Numeric[C]].toDouble),
      yr.toSeq.map(implicitly[Numeric[D]].toDouble))
  }

  def scatter[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.scatter)
    plot(addStyle(hc, xy))
  }

  def spline[C, D](xy: IterablePair[C, D]) = {
    val (xr, yr) = xy.toIterables
    val hc = xyToSeries(xr, yr, SeriesType.spline)
    plot(addStyle(hc, xy))
  }

  // Todo: can we disclose this information through reflection, instead of hardcoding it?
  /**
   * Output the basic usage of Highcharts
   */
  def help(): Unit = {
    println("\nAvailable Plot Types: Takes an Iterable, an Iterable of pairs, a pair of Iterables, or an Iterable and a Function\n")
    Seq("area", "areaspline", "bar", "column", "line", "pie", "scatter", "spline", "regression")
      .map(s => "\t" + s)
      .foreach(println)
    println("\nOther plotting options:\n")
    Map(
      "histogram" -> "Iterable of Numerics or Pairs",
      "boxplot" -> "Collections of five Numerics : low, q1, median, q3, high"
    )
      .map { case (plot, description) => "\t%-35s%s".format(plot, description)}
      .foreach(println)
    println("\nStylistic changes:\n")
    ListMap(
      List("hold") -> "plots the next plot on top of the existing plot",
      List("unhold") -> "plots the next plot in a new chart",
      List("title(String)") -> "add a title to the most recent plot",
      List("xAxis(String)") -> "adds a label to the x-axis",
      List(
        "xAxisType([",
        """"linear", "logarithmic",""",
        """"datetime", "category"""",
        "])"
      ) -> "updates the x-axis type",
      List("xAxisCategories(Iterable[String])") -> "create named labels for x-axis",
      List("yAxis(String)") -> "adds a label to y-axis",
      List(
        "yAxisType([",
        """"linear", "logarithmic",""",
        """"datetime", "category"""",
        "])"
      ) -> "updates the y-axis type",
      List("yAxisCategories(Iterable[String])") -> "create named labels for y-axis",
      List("legend(Iterable[String])") -> "adds a legend to the most recent plot",
      List( """stack(["normal", "percent"])""") -> "stacks bars, columns, and lines relative to each other",
      List("unstack") -> "remove stacking"
    ).foreach { case (methodLines, description) =>
      println("\t%-35s%s".format(methodLines(0), description))
      methodLines.slice(1, methodLines.length - 1).foreach(line => println("\t  %s".format(line)))
      methodLines.slice(1, methodLines.length).reverse.headOption.foreach(line => println("\t%s".format(line)))
    }

    println("\nServer Controls:\n")
    ListMap(
      "undo" -> "undoes the most recent action",
      "redo" -> "the opposite of undo",
      "delete" -> "wipes the most recent chart from the page",
      "deleteAll" -> "wipes all plots from the page"
    ).foreach { case (method, description) => println("\t%-35s%s".format(method, description))}
  }
}

