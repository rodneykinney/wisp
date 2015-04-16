package allenai

import spray.json.DefaultJsonProtocol._
import spray.json._

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
    xAxis: Array[Axis] = Array(Axis()),
    yAxis: Array[Axis] = Array(Axis()))
    extends HighchartElement {
  def updateXAxis(copier: Axis => Axis) = copy(xAxis = newAxis(xAxis, copier))

  def updateYAxis(copier: Axis => Axis) = copy(yAxis = newAxis(yAxis, copier))

  private def newAxis(axis: Array[Axis], copier: Axis => Axis) = {
    val original = if (axis.isEmpty) Array(Axis()) else axis
    original.map(copier)
  }

}

case class Chart(zoomType: String = "xy") extends HighchartElement

case class Exporting(enabled: Boolean = true) extends HighchartElement

case class Legend(
    x: Option[Int] = None,
    y: Option[Int] = None,
    title: String = "",
    enabled: Boolean = true
    ) extends HighchartElement

case class Series(
    data: Seq[Data]
    ) extends HighchartElement

sealed trait Data extends HighchartElement

case class NumericalData(x: Double, y: Double) extends Data

case class Title(
    text: String="",
    align: Align = Align.center
    ) extends HighchartElement

case class Axis(title: AxisTitle = AxisTitle()) extends HighchartElement

case class AxisTitle(text: String = "") extends HighchartElement


trait HighchartElement extends Product {
  val options = collection.mutable.Map[String, JsValue]()
  def addOption[T: JsonWriter](key: String, value: T): Unit = {
    options.update(key, value.toJson)
  }
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

object AllFormats {
  implicit def writerToFormat[T](writer: JsonWriter[T]) = new JsonFormat[T] {
    override def write(obj: T): JsValue = writer.write(obj)

    override def read(json: JsValue): T = ???
  }
  implicit val chart = HighchartElementJsonFormat(Chart)
  implicit val align = HighchartElementJsonFormat.asString[Align]
  implicit val title = HighchartElementJsonFormat(Title)
  implicit val axisTitle = HighchartElementJsonFormat(AxisTitle)
  implicit val axis: JsonFormat[Axis] = HighchartElementJsonFormat(Axis)
  implicit val exporting = HighchartElementJsonFormat(Exporting)
  implicit val legend = HighchartElementJsonFormat(Legend)
  implicit val data: JsonFormat[Data] = new JsonWriter[Data] {
    def write(obj: Data) = obj match {
      case n: NumericalData => HighchartElementJsonFormat(NumericalData).write(n)
    }
  }
  implicit val series: JsonFormat[Series] = HighchartElementJsonFormat(Series)
  implicit val highchartData = HighchartElementJsonFormat(HighchartAPI)
}


