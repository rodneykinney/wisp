package wisp.highcharts

import spray.json.JsValue
import wisp.{CustomJsonObject, Plotter}

import java.awt.Color
import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
trait RootPlot {
  var data: RootConfig
}


case class RootConfig(
                       chart: Chart = Chart(),
                       colors: Seq[Color] = null,
                       exporting: Exporting = Exporting(),
                       legend: Legend = Legend(),
                       series: IndexedSeq[Series] = Vector(),
                       subtitle: ChartTitle = null,
                       plotOptions: PlotOptions = null,
                       title: ChartTitle = ChartTitle(),
                       labels: FloatingLabels = null,
                       xAxis: IndexedSeq[Axis] = Vector(Axis()),
                       yAxis: IndexedSeq[Axis] = Vector(Axis()),
                       other: Map[String, JsValue] = Map())
  extends CustomJsonObject

class RootAPI(
               var data: RootConfig,
               plotter: Plotter[RootPlot, Int]) extends API with RootPlot {
  val index = plotter.addPlot(this)

  def update(newData: RootConfig) = {
    data = newData
    plotter.updatePlot(index, this)
    this
    //    this.asInstanceOf[T]
  }

  def remove() = plotter.removePlot(index)

  def xAxis(idx: Int): AxisAPI[RootAPI] = {
    val axis: Axis = data.xAxis(idx)
    axis.update { a =>
      update(data.copy(xAxis = data.xAxis.updated(idx, a)))
    }
  }

  def xAxis: AxisAPI[RootAPI] = xAxis(0)

  def yAxis(idx: Int): AxisAPI[RootAPI] = {
    val axis: Axis = data.yAxis(idx)
    axis.update { a =>
      update(data.copy(yAxis = data.yAxis.updated(idx, a)))
    }
  }

  def yAxis: AxisAPI[RootAPI] = yAxis(0)

  def addXAxis(axis: Axis = Axis()) = update(data.copy(xAxis = data.xAxis :+ axis))

  def addYAxis(axis: Axis = Axis()) = update(data.copy(yAxis = data.yAxis :+ axis))

  def stack(): RootAPI = stack(Stacking.normal)

  def stack(s: Stacking): RootAPI = update {

    val oldOptions = Option(data.plotOptions).getOrElse(PlotOptions())
    val oldSettings = Option(oldOptions.series).getOrElse(PlotSettings())
    data.copy(plotOptions =
      oldOptions.copy(series =
        oldSettings.copy(stacking
          = s)))
  }

  @WebMethod(action = "Size, borders, margins, etc.")
  def layout = data.chart.api(c => update(data.copy(chart = c)))

  @WebMethod(action = "Legend layout")
  def legend = data.legend.api(c => update(data.copy(legend = c)))

  @WebMethod(action = "Export to png, pdf, etc.")
  def exporting = data.exporting.api(e => update(data.copy(exporting = e)))

  @WebMethod(action = "Data series attributes")
  def series(idx: Int) = data.series(idx).api(s => update(data.copy(series = data.series.updated(idx, s))))

  @WebMethod(action = "Add new data series")
  def addSeries(xyData: SeriesData) = update {
    val oldSeries = data.series
    val seriesType = if (oldSeries.size > 0) oldSeries(0).`type` else SeriesType.line
    data.copy(series =
      oldSeries :+ Series(data = xyData.points, `type` = seriesType))
  }

  @WebMethod(action = "Title options")
  def title = data.title.api(t => update(data.copy(title = t)))

  @WebMethod(action = "Default colors for data series")
  def colors(x: Seq[Color]) = update(data.copy(colors = x))

  @WebMethod(action = "Add Text Label at (x,y) with CSS style")
  def addFloatingLabel(x: Int, y: Int, text: String, style: Map[String, String] = Map()) = {
    val oldLabels = Option(data.labels).getOrElse(FloatingLabels(Seq()))
    var fullStyle = style
    fullStyle += ("left" -> s"${x}px")
    fullStyle += ("top" -> s"${y}px")
    update(data.copy(labels = FloatingLabels(oldLabels.items :+ FloatingLabel(text, fullStyle))))
  }

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(data.copy(other = data.other + (name -> value)))
}

case class Exporting(enabled: Boolean = true,
                     other: Map[String, JsValue] = Map()) extends CustomJsonObject {
  def api[T](update: Exporting => T) = new ExportingAPI(this, update)
}

class ExportingAPI[T](e: Exporting, update: Exporting => T) extends API {
  def enabled(x: Boolean) = update(e.copy(enabled = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def other(name: String, value: JsValue) = update(e.copy(other = e.other + (name -> value)))
}

case class FloatingLabels(items: Seq[FloatingLabel])

case class FloatingLabel(html: String, style: Map[String, String])

