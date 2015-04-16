package allenai.highcharts

import java.awt.Color

import allenai._
import spray.json.DefaultJsonProtocol._
import spray.json._
import sun.jvm.hotspot.debugger.cdbg.EnumType

import scala.collection.mutable

/**
 * Created by rodneykinney on 4/15/15.
 */
case class HighchartAPI(
    chart: Chart = Chart(),
    exporting: Exporting = Exporting(),
    legend: Legend = Legend(),
    series: Seq[Series] = Seq(),
    subtitle: Title = null,
    title: Title = Title(),
    xAxis: IndexedSeq[Axis] = Vector(Axis()),
    yAxis: IndexedSeq[Axis] = Vector(Axis()),
    other: Map[String, JsValue] = Map())
    extends HighchartElement {
}

object HighchartAPI {
//  def pairs[T <% Double](data: Iterable[(T, T)], `type`: SeriesType) =
//    HighchartAPI(series = List(Series.pairs(data, `type`)))
//  def singleValue[T <% Double](data: Iterable[T], `type`: SeriesType) =
//    HighchartAPI(series = List(Series.singleValue(data, `type`)))
}

case class Chart(
    width: Int = 500,
    height: Int = 500,
    borderWidth: Int = 2,
    borderColor: Color = null,
    zoomType: String = "xy",
    other: Map[String, JsValue] = Map()
    ) extends HighchartElement {
  def update[T](update: Chart => T) = new {
    def width(x: Int) = update(copy(width = x))
    def height(x: Int) = update(copy(height = x))
    def borderWidth(x: Int) = update(copy(borderWidth = x))
    def borderColor(x: Color) = update(copy(borderColor = x))
    def zoomType(x: String) = update(copy(zoomType = x))
  }
}

case class Exporting(enabled: Boolean = true,
    other: Map[String, JsValue] = Map()) extends HighchartElement {
  def update[T](update: Exporting => T) = new {
    def enabled(x: Boolean) = update(copy(enabled = x))
  }
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
    ) extends HighchartElement

object Series {
//  def pairs[T <% Double](data: Iterable[(T, T)], `type`: SeriesType) =
//    new Series(data = data.toSeq.map { case (a, b) => RichPoint(a.toDouble, b.toDouble) }, `type` = `type`)
//  def singleValue[T <% Double](data: Iterable[T], `type`: SeriesType) =
//    new Series(data = data.toSeq.map(x => YValue(x.toDouble)), `type` = `type`)
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
  def update[T](update: Title => T) = new {
    def text(x: String) = update(copy(text = x))
    def align(x: Align) = update(copy(align=x))
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

class AxisAPI[T](axis:Axis)(update: Axis => T) {
  def axisType(x: AxisType) = update(axis.copy(`type` = x))
  def title = axis.title.update(t => update(axis.copy(title = t)))
  def categories(x: Iterable[String])= update(axis.copy(categories = x.toIndexedSeq))
  def min(min: Double) = update(axis.copy(min = Some(min)))
  def max(max: Double) = update(axis.copy(max = Some(max)))
}

case class AxisTitle(text: String = "",
    other: Map[String, JsValue] = Map()) extends HighchartElement {
  def update[T](update: AxisTitle => T) = new {
    def text(x: String) = update(copy(text = x))
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


object AllFormats {
  implicit def writerToFormat[T](writer: JsonWriter[T]) = new JsonFormat[T] {
    override def write(obj: T): JsValue = writer.write(obj)

    override def read(json: JsValue): T = ???
  }

  implicit val color =
    new JsonWriter[Color] {
      def write(c: Color) = "#%02x%02x%02x".format(c.getRed, c.getGreen, c.getBlue).toJson
    }
  implicit val chart = HighchartElementJsonFormat(Chart)
  implicit val align = HighchartElementJsonFormat.asString[Align]
  implicit val title = HighchartElementJsonFormat(Title)
  implicit val axisTitle = HighchartElementJsonFormat(AxisTitle)
  implicit val axisType = HighchartElementJsonFormat.asString[AxisType]
  implicit val axis: JsonFormat[Axis] = HighchartElementJsonFormat(Axis)
  implicit val exporting = HighchartElementJsonFormat(Exporting)
  implicit val legend = HighchartElementJsonFormat(Legend)
  implicit val dataLabels = HighchartElementJsonFormat(DataLabels)
  implicit val richPoint = HighchartElementJsonFormat(RichPoint)
  implicit val data: JsonFormat[Point] = new JsonWriter[Point] {
    def write(obj: Point) = obj match {
      case n: XYValue => (n.x, n.y).toJson
      case n: YValue => n.value.toJson
      case p: RichPoint => richPoint.write(p)
    }
  }
  implicit val seriesType: JsonFormat[SeriesType] = HighchartElementJsonFormat.asString[SeriesType]
  implicit val series: JsonFormat[Series] = HighchartElementJsonFormat(Series.apply _)
  implicit val highchartData = HighchartElementJsonFormat(HighchartAPI.apply _)
}


