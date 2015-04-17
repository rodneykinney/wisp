package wisp.highcharts

import java.awt.Color
import javax.jws.WebMethod

import wisp._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.annotation.StaticAnnotation

/**
 * Created by rodneykinney on 4/15/15.
 */
case class Highchart(
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
    other: Map[String, JsValue] = Map("credits" -> Map("enabled" -> "false").toJson))
    extends HighchartElement {
  def api[T](update: Highchart => T) = new API {
    @WebMethod(action = "Size, borders, margins, etc.")
    def layout = chart.api(c => update(copy(chart = c)))
    @WebMethod(action = "Legend layout")
    def legend = Highchart.this.legend.api(c => update(copy(legend = c)))
    @WebMethod(action = "Export to png, pdf, etc.")
    def exporting = Highchart.this.exporting.api(e => update(copy(exporting = e)))
    @WebMethod(action = "Series data attributes")
    def series(idx: Int) = Highchart.this.series(idx).api(s => update(copy(series = Highchart.this.series.updated(idx, s))))
    @WebMethod(action = "Add new data series")
    def addSeries(xyData: SeriesData) = update {
      val oldSeries = Highchart.this.series
      val seriesType = if (oldSeries.size > 0) oldSeries(0).`type` else SeriesType.line
      Highchart.this.copy(series =
          oldSeries :+ Series(data = xyData.points, `type` = seriesType))
    }
    @WebMethod(action = "Title options")
    def title = Highchart.this.title.api(t => update(Highchart.this.copy(title = t)))
    @WebMethod(action = "Default colors for data series")
    def colors(x: Seq[Color]) = update(copy(colors = x))
    @WebMethod(action = "Add Text Label at (x,y) with CSS style")
    def addFloatingLabel(x: Int, y: Int, text: String, style: Map[String, String] = Map()) = {
      val oldLabels = Option(labels).getOrElse(FloatingLabels(Seq()))
      var fullStyle = style
      fullStyle += ("left" -> s"${x}px")
      fullStyle += ("top" -> s"${y}px")
      update(copy(labels = FloatingLabels(oldLabels.items :+ FloatingLabel(text, fullStyle))))
    }
  }
}

case class FloatingLabels(items: Seq[FloatingLabel])
case class FloatingLabel(html: String, style: Map[String, String])

case class Chart(
    width: Int = 500,
    height: Int = 500,
    borderWidth: Int = 2,
    borderColor: Color = null,
    backgroundColor: Color = null,
    borderRadius: Option[Int] = None,
    margin: Option[(Int, Int, Int, Int)] = None,
    plotBackgroundColor: Color = null,
    plotBorderColor: Color = null,
    plotBorderWidth: Option[Int] = None,
    plotShadow: Option[Boolean] = None,
    polar: Option[Boolean] = None,
    style: Map[String, String] = null,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def api[T](update: Chart => T) = new API {
    def size(w: Int, h: Int) = update(copy(width = w, height = h))
    def borderColor(x: Color) = update(copy(borderColor = x))
    def backgroundColor(x: Color) = update(copy(backgroundColor = x))
    @WebMethod(action = "Corner radius for chart border")
    def borderRadius(x: Int) = update(copy(borderRadius = Some(x)))
    @WebMethod(action = "Pixel width of chart border")
    def borderWidth(x: Int) = update(copy(borderWidth = x))
    @WebMethod(action = "Outer margin: top, right, bottom, left")
    def margin(top: Int, right: Int, bottom: Int, left: Int) = update(copy(margin = Some((top, right, bottom, left))))
    @WebMethod(action = "Background color of the plot area")
    def plotBackgroundColor(x: Color) = update(copy(plotBackgroundColor = x))
    @WebMethod(action = "Color of the plot area")
    def plotBorderColor(x: Color) = update(copy(plotBorderColor = x))
    @WebMethod(action = "Pixel width of plot area border")
    def plotBorderWidth(x: Int) = update(copy(plotBorderWidth = Some(x)))
    @WebMethod(action = "Show shadow (background color must be set)")
    def plotShadow(x: Boolean) = update(copy(plotShadow = Some(x)))
    @WebMethod(action = "Use polar coordinates (r, theta)")
    def polar(x: Boolean) = update(copy(polar = Some(x)))
    @WebMethod(action = "CSS style")
    def style(x: Map[String, String]) = update(copy(style = x))
  }
}

case class Exporting(enabled: Boolean = true,
    other: Map[String, JsValue] = Map()) extends HighchartElement {
  def api[T](update: Exporting => T) = new API {
    def enabled(x: Boolean) = update(copy(enabled = x))
  }
}

case class PlotOptions(
    area: PlotSettings = null,
    areaspline: PlotSettings = null,
    bar: PlotSettings = null,
    column: PlotSettings = null,
    line: PlotSettings = null,
    other: Map[String, JsValue] = Map()) extends HighchartElement

case class PlotSettings(
    stacking: Stacking = null,
    shadow: Option[Boolean] = None,
    other: Map[String, JsValue] = Map()) extends HighchartElement

sealed trait Stacking extends EnumTrait
object Stacking {
  case object normal extends Stacking
  case object percent extends Stacking
}

case class Legend(
    x: Option[Int] = None,
    y: Option[Int] = None,
    title: LegendTitle = null,
    enabled: Boolean = true,
    align: HAlign = null,
    borderWidth: Int = 2,
    borderColor: Color = null,
    backgroundColor: Color = null,
    borderRadius: Option[Int] = None,
    floating: Option[Boolean] = None,
    layout: Orientation = null,
    shadow: Option[Boolean] = None,
    verticalAlign: VAlign = null,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def api[T](update: Legend => T) = new API {
    @WebMethod(action = "(x,y) position")
    def position(x: Int, y: Int) = update(copy(x = Some(x), y = Some(y)))
    def enabled(x: Boolean) = update(copy(enabled = x))
    @WebMethod(action = "Allow legend to overlap plot area")
    def floating(x: Boolean) = update(copy(floating = Some(x)))
    def horizontalJustification(x: HAlign) = update(copy(align = x))
    def borderColor(x: Color) = update(copy(borderColor = x))
    def backgroundColor(x: Color) = update(copy(backgroundColor = x))
    def borderRadius(x: Int) = update(copy(borderRadius = Some(x)))
    def borderWidth(x: Int) = update(copy(borderWidth = x))
    @WebMethod(action = "horizontal/vertical layout")
    def layout(x: Orientation) = update(copy(layout = x))
    @WebMethod(action = "Show shadow (background color must be set)")
    def shadow(x: Boolean) = update(copy(shadow = Some(x)))
    @WebMethod(action = "Title text and CSS style")
    def title(text: String, style: Map[String,String] = Map()) = update(copy(title = LegendTitle(text, style)))
    def verticalJustification(x: VAlign) = update(copy(verticalAlign = x))
  }
}

case class LegendTitle(text: String, style: Map[String, String] = Map())

case class Series(
    data: Seq[Point],
    name: String = "",
    `type`: SeriesType,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def api[T](update: Series => T) = new API {
    def name(s: String) = update(copy(name = s))
  }
}

sealed trait Point extends HighchartElement

case class RichPoint(x: Option[Double] = None,
    y: Option[Double] = None,
    color: Color = null,
    dataLabels: DataLabels = null,
    name: String = null,
    other: Map[String, JsValue] = Map()) extends Point
case class XYValue(x: Double, y: Double,
    other: Map[String, JsValue] = Map()) extends Point
case class YValue(value: Double,
    other: Map[String, JsValue] = Map()) extends Point

case class DataLabels(x: Option[Int] = None,
    y: Option[Int] = None,
    other: Map[String, JsValue] = Map()) extends HighchartElement

case class ChartTitle(
    text: String = "",
    align: HAlign = HAlign.center,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def api[T](update: ChartTitle => T) = new API {
    def text(x: String) = update(copy(text = x))
    @WebMethod(action = "Align.[left|right|center]")
    def align(x: HAlign) = update(copy(align = x))
  }

}

case class Axis(
    title: AxisTitle = AxisTitle(),
    `type`: AxisType = AxisType.linear,
    categories: IndexedSeq[String] = null,
    min: Option[Double] = None,
    max: Option[Double] = None,
    other: Map[String, JsValue] = Map()) extends HighchartElement {
  def update[T](update: Axis => T) = new AxisAPI(this)(update)
}

class AxisAPI[T](axis: Axis)(update: Axis => T) extends API {
  def axisType(x: AxisType) = update(axis.copy(`type` = x))
  def title = axis.title.update(t => update(axis.copy(title = t)))
  def categories(x: Iterable[String]) = update(axis.copy(categories = x.toIndexedSeq))
  def range(min: Double, max: Double) = update(axis.copy(min = Some(min), max = Some(max)))
}

case class AxisTitle(text: String = "",
    other: Map[String, JsValue] = Map()) extends HighchartElement {
  def update[T](update: AxisTitle => T) = new API {
    def text(x: String) = update(copy(text = x))
  }
}

trait API {
  def help = {
    for {
      m <- this.getClass.getDeclaredMethods
      method = m.getName
      if (method.indexOf('$') < 0 && method != "help")
    } {
      val msg = Option(m.getAnnotation(classOf[WebMethod])).map(s => s" -- ${s.action}").getOrElse("")
      val params = m.getParameterTypes.map(_.getSimpleName) match {
        case Array() => ""
        case a => a.mkString("(", ",", ")")
      }
      println(s"${m.getName}$params$msg")
    }
  }
}

trait HighchartElement extends Product {
  val other: Map[String, JsValue]
}

