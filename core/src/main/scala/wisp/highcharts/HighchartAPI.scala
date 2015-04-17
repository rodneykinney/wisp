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
case class HighchartAPI(
    chart: Chart = Chart(),
    exporting: Exporting = Exporting(),
    legend: Legend = Legend(),
    series: IndexedSeq[Series] = Vector(),
    subtitle: Title = null,
    plotOptions: PlotOptions = null,
    title: Title = Title(),
    xAxis: IndexedSeq[Axis] = Vector(Axis()),
    yAxis: IndexedSeq[Axis] = Vector(Axis()),
    other: Map[String, JsValue] = Map())
    extends HighchartElement {
  def update[T](update: HighchartAPI => T) = new API {
    @WebMethod(action = "Size, borders, margins, etc.")
    def layout = chart.update(c => update(copy(chart = c)))
    @WebMethod(action = "Export to png, pdf, etc.")
    def exporting = HighchartAPI.this.exporting.update(e => update(copy(exporting = e)))
    @WebMethod(action = "Series data attributes")
    def series(idx: Int) = HighchartAPI.this.series(idx).update(s => update(copy(series = HighchartAPI.this.series.updated(idx, s))))
    @WebMethod(action = "Add new data series")
    def addSeries(xyData: SeriesData) = update {
      val oldSeries = HighchartAPI.this.series
      val seriesType = if (oldSeries.size > 0) oldSeries(0).`type` else SeriesType.line
      HighchartAPI.this.copy(series =
          oldSeries :+ Series(data = xyData.points, `type` = seriesType))
    }
    @WebMethod(action = "Title options")
    def title = HighchartAPI.this.title.update(t => update(HighchartAPI.this.copy(title = t)))
  }
}

case class Chart(
    width: Int = 500,
    height: Int = 500,
    borderWidth: Int = 2,
    borderColor: Color = null,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def update[T](update: Chart => T) = new {
    def size(w: Int, h: Int) = update(copy(width = w, height = h))
    def borderWidth(x: Int) = update(copy(borderWidth = x))
    def borderColor(x: Color) = update(copy(borderColor = x))
  }
}

case class Exporting(enabled: Boolean = true,
    other: Map[String, JsValue] = Map()) extends HighchartElement {
  def update[T](update: Exporting => T) = new API {
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
    title: String = "",
    enabled: Boolean = true,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement

case class Series(
    data: Seq[Point],
    name: String = "",
    `type`: SeriesType,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def update[T](update: Series => T) = new API {
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

case class Title(
    text: String = "",
    align: Align = Align.center,
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def update[T](update: Title => T) = new API {
    def text(x: String) = update(copy(text = x))
    def align(x: Align) = update(copy(align = x))
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

trait EnumTrait {
  override def toString = this.getClass.getSimpleName.stripSuffix("$")
}

sealed trait Align extends EnumTrait

object Align {
  case object left extends Align
  case object center extends Align
  case object right extends Align
}

sealed trait AxisType extends EnumTrait

object AxisType {
  case object category extends AxisType
  case object datetime extends AxisType
  case object linear extends AxisType
  case object logarithmic extends AxisType
}

sealed trait SeriesType extends EnumTrait

object SeriesType {
  case object area extends SeriesType
  case object areaspline extends SeriesType
  case object bar extends SeriesType
  case object boxplot extends SeriesType
  case object column extends SeriesType
  case object line extends SeriesType
  case object pie extends SeriesType
  case object scatter extends SeriesType
  case object spline extends SeriesType
}



