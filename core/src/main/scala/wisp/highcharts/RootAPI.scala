package wisp.highcharts

import spray.json.JsValue
import wisp.{CustomJsonObject, ChartDisplay}

import java.awt.Color
import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
case class RootConfig(
                       chart: Chart = Chart(),
                       colors: Seq[Color] = null,
                       exporting: Exporting = Exporting(),
                       legend: Legend = Legend(),
                       series: IndexedSeq[Series] = Vector(),
                       subtitle: ChartTitle = null,
                       plotOptions: PlotSpecificSettings = null,
                       title: ChartTitle = ChartTitle(),
                       labels: FloatingLabels = null,
                       xAxis: IndexedSeq[Axis] = Vector(Axis()),
                       yAxis: IndexedSeq[Axis] = Vector(Axis()),
                       other: Map[String, JsValue] = Map())
  extends CustomJsonObject

trait RootChart {
  var config: RootConfig
}

trait BaseAPI[T <: BaseAPI[T]] extends RootChart with API {
  val display: ChartDisplay[RootChart, Int]

  val index = display.addChart(this)

  def update(newData: RootConfig): T = {
    config = newData
    display.updateChart(index, this)
    this.asInstanceOf[T]
  }
}

class GenericChartAPI(var config: RootConfig,
                      val display: ChartDisplay[RootChart, Int])
  extends RootAPI[GenericChartAPI]

trait RootAPI[T <: BaseAPI[T]] extends BaseAPI[T] {
  @WebMethod(action = "Remove this chart from view")
  def remove() = display.removeChart(index)

  @WebMethod
  def getXAxis(idx: Int) = {
    val axis: Axis = config.xAxis(idx)
    axis.api { a =>
      update(config.copy(xAxis = config.xAxis.updated(idx, a)))
    }
  }

  @WebMethod
  def xAxis = getXAxis(0)

  @WebMethod
  def getYAxis(idx: Int) = {
    val axis: Axis = config.yAxis(idx)
    axis.api { a =>
      update(config.copy(yAxis = config.yAxis.updated(idx, a)))
    }
  }

  @WebMethod
  def yAxis = getYAxis(0)

  @WebMethod
  def addXAxis(axis: Axis = Axis()) = update(config.copy(xAxis = config.xAxis :+ axis))

  @WebMethod
  def addYAxis(axis: Axis = Axis()) = update(config.copy(yAxis = config.yAxis :+ axis))

  @WebMethod(action = "Settings that apply to all data series on this chart")
  def defaultSettings = {
    val oldPlotOptions = Option(config.plotOptions).getOrElse(PlotSpecificSettings())
    val series = Option(oldPlotOptions.series).getOrElse(SeriesSettings())
    series.api(s => update(config.copy(plotOptions = oldPlotOptions.copy(series = s))))
  }

  @WebMethod(action = "Size, borders, margins, etc.")
  def layout = config.chart.api(c => update(config.copy(chart = c)))

  @WebMethod(action = "Legend layout")
  def legend = config.legend.api(c => update(config.copy(legend = c)))

  @WebMethod(action = "Export to png, pdf, etc.")
  def exporting = config.exporting.api(e => update(config.copy(exporting = e)))

  @WebMethod(action = "Data series attributes")
  def series(idx: Int) = config.series(idx).api(s => update(config.copy(series = config.series.updated(idx, s))))

  @WebMethod(action = "Add new data series")
  def addSeries(xyData: SeriesData) = update {
    val oldSeries = config.series
    val seriesType = if (oldSeries.size > 0) oldSeries(0).`type` else SeriesType.line
    config.copy(series =
      oldSeries :+ Series(data = xyData.points, `type` = seriesType))
  }

  @WebMethod(action = "Title options")
  def title = config.title.api(t => update(config.copy(title = t)))

  @WebMethod(action = "Default colors for data series")
  def colors(x: Seq[Color]) = update(config.copy(colors = x))

  @WebMethod(action = "Add Text Label at (x,y) with CSS style")
  def addFloatingLabel(x: Int, y: Int, text: String, style: Map[String, String] = Map()) = {
    val oldLabels = Option(config.labels).getOrElse(FloatingLabels(Seq()))
    var fullStyle = style
    fullStyle += ("left" -> s"${x}px")
    fullStyle += ("top" -> s"${y}px")
    update(config.copy(labels = FloatingLabels(oldLabels.items :+ FloatingLabel(text, fullStyle))))
  }

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(config.copy(other = config.other + (name -> value)))
}

case class Exporting(enabled: Boolean = true,
                     other: Map[String, JsValue] = Map()) extends CustomJsonObject {
  def api[T](update: Exporting => T) = new ExportingAPI(this, update)
}

class ExportingAPI[T](e: Exporting, update: Exporting => T) extends API {
  @WebMethod
  def enabled(x: Boolean) = update(e.copy(enabled = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(e.copy(other = e.other + (name -> value)))
}

case class FloatingLabels(items: Seq[FloatingLabel])

case class FloatingLabel(html: String, style: Map[String, String])

