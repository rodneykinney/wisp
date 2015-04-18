package wisp.highcharts

import spray.json.DefaultJsonProtocol._
import spray.json._

import java.awt.Color
import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/15/15.
 */
case class HighchartsRoot(
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
  def api[T](update: HighchartsRoot => T) = new RootAPI(this, update)
}

class RootAPI[T](root: HighchartsRoot, update: HighchartsRoot => T) extends API {
  @WebMethod(action = "Size, borders, margins, etc.")
  def layout = root.chart.api(c => update(root.copy(chart = c)))

  @WebMethod(action = "Legend layout")
  def legend = root.legend.api(c => update(root.copy(legend = c)))

  @WebMethod(action = "Export to png, pdf, etc.")
  def exporting = root.exporting.api(e => update(root.copy(exporting = e)))

  @WebMethod(action = "Series data attributes")
  def series(idx: Int) = root.series(idx).api(s => update(root.copy(series = root.series.updated(idx, s))))

  @WebMethod(action = "Add new data series")
  def addSeries(xyData: SeriesData) = update {
    val oldSeries = root.series
    val seriesType = if (oldSeries.size > 0) oldSeries(0).`type` else SeriesType.line
    root.copy(series =
      oldSeries :+ Series(data = xyData.points, `type` = seriesType))
  }

  @WebMethod(action = "Title options")
  def title = root.title.api(t => update(root.copy(title = t)))

  @WebMethod(action = "Default colors for data series")
  def colors(x: Seq[Color]) = update(root.copy(colors = x))

  @WebMethod(action = "Add Text Label at (x,y) with CSS style")
  def addFloatingLabel(x: Int, y: Int, text: String, style: Map[String, String] = Map()) = {
    val oldLabels = Option(root.labels).getOrElse(FloatingLabels(Seq()))
    var fullStyle = style
    fullStyle += ("left" -> s"${x}px")
    fullStyle += ("top" -> s"${y}px")
    update(root.copy(labels = FloatingLabels(oldLabels.items :+ FloatingLabel(text, fullStyle))))
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
  def api[T](update: Chart => T) = new ChartAPI(this, update)
}

class ChartAPI[T](chart: Chart, update: Chart => T) extends API {
  def size(w: Int, h: Int) = update(chart.copy(width = w, height = h))

  def borderColor(x: Color) = update(chart.copy(borderColor = x))

  def backgroundColor(x: Color) = update(chart.copy(backgroundColor = x))

  @WebMethod(action = "Corner radius for chart border")
  def borderRadius(x: Int) = update(chart.copy(borderRadius = Some(x)))

  @WebMethod(action = "Pixel width of chart border")
  def borderWidth(x: Int) = update(chart.copy(borderWidth = x))

  @WebMethod(action = "Outer margin: top, right, bottom, left")
  def margin(top: Int, right: Int, bottom: Int, left: Int) = update(chart.copy(margin = Some((top, right, bottom, left))))

  @WebMethod(action = "Background color of the plot area")
  def plotBackgroundColor(x: Color) = update(chart.copy(plotBackgroundColor = x))

  @WebMethod(action = "Color of the plot area")
  def plotBorderColor(x: Color) = update(chart.copy(plotBorderColor = x))

  @WebMethod(action = "Pixel width of plot area border")
  def plotBorderWidth(x: Int) = update(chart.copy(plotBorderWidth = Some(x)))

  @WebMethod(action = "Show shadow (background color must be set)")
  def plotShadow(x: Boolean) = update(chart.copy(plotShadow = Some(x)))

  @WebMethod(action = "Use polar coordinates (r, theta)")
  def polar(x: Boolean) = update(chart.copy(polar = Some(x)))

  @WebMethod(action = "CSS style")
  def style(x: Map[String, String]) = update(chart.copy(style = x))
}


case class Exporting(enabled: Boolean = true,
                     other: Map[String, JsValue] = Map()) extends HighchartElement {
  def api[T](update: Exporting => T) = new ExportingAPI(this, update)
}

class ExportingAPI[T](e: Exporting, update: Exporting => T) extends API {
  def enabled(x: Boolean) = update(e.copy(enabled = x))
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
  def api[T](update: Legend => T) = new LegendAPI(this, update)
}

class LegendAPI[T](legend: Legend, update: Legend => T) extends API {
  @WebMethod(action = "(x,y) position")
  def position(x: Int, y: Int) = update(legend.copy(x = Some(x), y = Some(y)))

  def enabled(x: Boolean) = update(legend.copy(enabled = x))

  @WebMethod(action = "Allow legend to overlap plot area")
  def floating(x: Boolean) = update(legend.copy(floating = Some(x)))

  def horizontalJustification(x: HAlign) = update(legend.copy(align = x))

  def borderColor(x: Color) = update(legend.copy(borderColor = x))

  def backgroundColor(x: Color) = update(legend.copy(backgroundColor = x))

  def borderRadius(x: Int) = update(legend.copy(borderRadius = Some(x)))

  def borderWidth(x: Int) = update(legend.copy(borderWidth = x))

  @WebMethod(action = "horizontal/vertical layout")
  def layout(x: Orientation) = update(legend.copy(layout = x))

  @WebMethod(action = "Show shadow (background color must be set)")
  def shadow(x: Boolean) = update(legend.copy(shadow = Some(x)))

  @WebMethod(action = "Title text and CSS style")
  def title(text: String, style: Map[String, String] = Map()) = update(legend.copy(title = LegendTitle(text, style)))

  def verticalJustification(x: VAlign) = update(legend.copy(verticalAlign = x))
}


case class LegendTitle(text: String, style: Map[String, String] = Map())

case class Series(
                   data: Seq[Point],
                   name: String = "",
                   `type`: SeriesType,
                   other: Map[String, JsValue] = Map()
                   ) extends HighchartElement {
  def api[T](update: Series => T) = new SeriesAPI(this, update)
}

class SeriesAPI[T](series: Series, update: Series => T) extends API {
  def name(s: String) = update(series.copy(name = s))
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
  def api[T](update: ChartTitle => T) = new ChartTitleAPI(this, update)
}

class ChartTitleAPI[T](ct: ChartTitle, update: ChartTitle => T) extends API {
  def text(x: String) = update(ct.copy(text = x))

  @WebMethod(action = "Align.[left|right|center]")
  def align(x: HAlign) = update(ct.copy(align = x))
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
  def update[T](update: AxisTitle => T) = new AxisTitleAPI(this, update)
}

class AxisTitleAPI[T](at: AxisTitle, update: AxisTitle => T) extends API {
  def text(x: String) = update(at.copy(text = x))
}


trait API {
  def help = {
    val methodDescriptions = for {
      m <- this.getClass.getDeclaredMethods
      method = m.getName
      if (method.indexOf('$') < 0 && method != "help")
    } yield {
      val msg = Option(m.getAnnotation(classOf[WebMethod])).map(s => s" -- ${s.action}").getOrElse("")
      val params = m.getParameterTypes.map(_.getSimpleName) match {
        case Array() => ""
        case a => a.mkString("(", ",", ")")
      }
      s"${m.getName}$params$msg"
    }
    for (line <- methodDescriptions.sorted) {
      println(line)
    }
  }
}

trait HighchartElement extends Product {
  val other: Map[String, JsValue]
}

