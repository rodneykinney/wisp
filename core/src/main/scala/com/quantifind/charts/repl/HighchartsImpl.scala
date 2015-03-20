package com.quantifind.charts.repl

import java.io.{ PrintWriter, File }

import com.quantifind.charts.highcharts.{ Series, SeriesType, Highchart }
import com.quantifind.charts.highcharts._
import scala.concurrent.Promise
import scala.util.Random
import org.apache.commons.io.FileUtils
/**
 * User: austin
 * Date: 12/3/14
 */
trait WebPlotHighcharts extends WebPlot[Highchart] {

  /**
   * Iterates through the plots and builds the necessary javascript and html around them.
   * returns the files contents as a string
   */
  def buildHtmlFile(): String = {
    val sb = new StringBuilder()
    sb.append(jsHeader)
    sb.append(reloadJs)
    sb.append("</head>")
    sb.append("<body>")
    plots.map(highchartsContainer).foreach(sb.append)
    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  def plotAll(): Unit = {

    val fileContents = buildHtmlFile()

    val temp = File.createTempFile("highcharts", ".html")
    val pw = new PrintWriter(temp)
    pw.print(fileContents)
    pw.flush()
    pw.close()

    plotServer.foreach { ps =>
      ps.p.success(())
      ps.p = Promise[Unit]()
    }

    val (serverRootFile, port, serverMode) = getWispServerInfo()

    def link =
      if (serverMode) {
        if (!serverRootFile.exists())
          FileUtils.moveFile(temp, serverRootFile)
        s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}"
      } else s"file://$temp"

    openFirstWindow(link)

    println(s"Output written to $link (CMD + Click link in Mac OSX).")
  }

  override def plot(t: Highchart): Highchart = {
    super.plot(t)
    plots = t +: plots
    undoStack.push(plots)
    plotAll()
    t
  }

  def reloadJs =
    """
      |<script type="text/javascript">$.ajax({url: '/check', dataType: 'jsonp', complete: function(){location.reload()}})</script>
    """.stripMargin

  val wispJsImports: String =
    """
      |<script type="text/javascript" src="http://code.jquery.com/jquery-1.8.2.min.js"></script>
      |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/highcharts.js"></script>
      |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/modules/exporting.js"></script>
    """.stripMargin

  val jsHeader =
    """
      |<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
      |<html>
      |  <head>
      |    <title>
      |      Highchart
      |    </title>
      |    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    """.stripMargin +
      wispJsImports

  def highchartsContainer(hc: Highchart): String = {
    val hash = hc.hashCode()
    val containerId = Random.nextInt(1e10.toInt) + (if (hash < 0) -1 else 1) * hash // salt the hash to allow duplicates
    highchartsContainer(hc.toJson, containerId)
  }

  def highchartsContainer(json: String, index: Int): String =
    containerDivs(index) + "\n" +
      """
      |    <script type="text/javascript">
      |        $(function() {
      |            $('#container%s').highcharts(
    """.stripMargin.format(index.toString) +
      """
      |                %s
      |            );
      |        });
      |    </script>
      |
    """.stripMargin.format(json)

  def containerDivs(index: Int) =
    s"""
      |    <div id="container%s" style="min-width: 400px; height: 400px; margin: 0 auto"></div>
    """.stripMargin.format(index.toString)
}

/**
 * Defines auxiliary tools available to plots, such as adding a Title
 */
trait HighchartsStyles extends Hold[Highchart] with Labels[Highchart] with WebPlotHighcharts {
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
  def xAxis(label: String): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(xAxis = plot.xAxis.map {
      axisArray => axisArray.map { _.copy(title = label) }
    })
    super.plot(newPlot)
  }
  def xAxisType(axisType: AxisType.Type): Highchart = {
    val plot = plots.head
    if (!AxisType.values.contains(axisType)) {
      println(s"Not an acceptable axis type. Options are: ${AxisType.values.mkString(", ")}.")
      return plot
    }
    plots = plots.tail
    val newPlot = plot.copy(xAxis = plot.xAxis.map {
      axisArray => axisArray.map { _.copy(axisType = axisType) }
    })
    super.plot(newPlot)
  }
  def yAxis(label: String): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(yAxis = plot.yAxis.map {
      axisArray => axisArray.map { _.copy(title = label) }
    })
    super.plot(newPlot)
  }
  def yAxisType(axisType: AxisType.Type): Highchart = {
    val plot = plots.head
    if (!AxisType.values.contains(axisType)) {
      println(s"Not an acceptable axis type. Options are: ${AxisType.values.mkString(", ")}.")
      return plot
    }
    plots = plots.tail
    val newPlot = plot.copy(yAxis = plot.yAxis.map {
      axisArray => axisArray.map { _.copy(axisType = axisType) }
    })
    super.plot(newPlot)
  }
  def title(label: String): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(title = label)
    super.plot(newPlot)
  }
  // Assign names to series, if mis-matched lengths use the shorter one as a cut-off
  def legend(labels: Iterable[String]): Highchart = {
    val labelArray = labels.toArray
    val plot = plots.head
    plots = plots.tail
    val newSeries = plot.series.toSeq.zipWithIndex.map { case (s, idx) => if (idx >= labels.size) s else s.copy(name = Some(labelArray(idx))) }
    val newPlot = plot.copy(series = newSeries)
    super.plot(newPlot)
  }
  def xyToSeries[T1: Numeric, T2: Numeric](x: Iterable[T1], y: Iterable[T2], chartType: SeriesType.Type) =
    plot(Highchart(Series(x.zip(y).toSeq, chart = chartType)))

  def stack(stackType: Stacking.Type = Stacking.normal): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(plotOptions = Some(PlotOptions(series = PlotOptionKey(stacking = stackType))))
    super.plot(newPlot)
  }

  def unstack(): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(plotOptions = Some(PlotOptions(series = PlotOptionKey(stacking = None))))
    super.plot(newPlot)
  }
}
