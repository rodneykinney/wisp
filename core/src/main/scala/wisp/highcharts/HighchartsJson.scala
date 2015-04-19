package wisp.highcharts

import java.awt.Color
import java.lang.reflect.Modifier
import wisp.{CustomJsonFormat, CustomJsonObject}

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.util.control.NonFatal

/**
 * Created by rodneykinney on 4/15/15.
 */
object HighchartsJson {
  implicit def writerToFormat[T](writer: JsonWriter[T]) = new JsonFormat[T] {
    import wisp.CustomJsonFormat._
    override def write(obj: T): JsValue = writer.write(obj)

    override def read(json: JsValue): T = ???
  }

  implicit val color: JsonFormat[Color] =
    new JsonWriter[Color] {
      def write(c: Color) = "#%02x%02x%02x".format(c.getRed, c.getGreen, c.getBlue).toJson
    }
  implicit val chart: JsonFormat[Chart] = CustomJsonFormat.apply(Chart)
  implicit val hAlign: JsonFormat[HAlign] = CustomJsonFormat.asString[HAlign]
  implicit val vAlign: JsonFormat[VAlign] = CustomJsonFormat.asString[VAlign]
  implicit val title: JsonFormat[ChartTitle] = CustomJsonFormat(ChartTitle)
  implicit val axisTitle: JsonFormat[AxisTitle] = CustomJsonFormat(AxisTitle)
  implicit val axisType: JsonFormat[AxisType] = CustomJsonFormat.asString[AxisType]
  implicit val axis: JsonFormat[Axis] = CustomJsonFormat(Axis)
  implicit val exporting: JsonFormat[Exporting] = CustomJsonFormat(Exporting)
  implicit val orientation: JsonFormat[Orientation] = CustomJsonFormat.asString[Orientation]
  implicit val legendTitle = jsonFormat2(LegendTitle)
  implicit val legend: JsonFormat[Legend] = CustomJsonFormat(Legend)
  implicit val dataLabels: JsonFormat[DataLabel] = CustomJsonFormat(DataLabel)
  implicit val richPoint: JsonFormat[RichPoint] = CustomJsonFormat(RichPoint)
  implicit val stacking: JsonFormat[Stacking] = CustomJsonFormat.asString[Stacking]
  implicit val dashStyle: JsonFormat[DashStyle] = CustomJsonFormat.asString[DashStyle]
  implicit val markerSymbol: JsonFormat[MarkerSymbol] = CustomJsonFormat.asString[MarkerSymbol]
  implicit val marker: JsonFormat[MarkerConfig] = CustomJsonFormat(MarkerConfig)
  implicit val plotSettings: JsonFormat[SeriesSettings] = CustomJsonFormat(SeriesSettings)
  implicit val plotOptions: JsonFormat[PlotSpecificSettings] = CustomJsonFormat(PlotSpecificSettings)
  implicit val data: JsonFormat[Point] = new JsonWriter[Point] {
    def write(obj: Point) = obj match {
      case n: XYValue => (n.x, n.y).toJson
      case n: YValue => n.value.toJson
      case p: RichPoint => richPoint.write(p)
    }
  }
  implicit val seriesType: JsonFormat[SeriesType] = CustomJsonFormat.asString[SeriesType]
  implicit val series: JsonFormat[Series] = CustomJsonFormat(Series)
  implicit val floatingLabel = jsonFormat2(FloatingLabel)
  implicit val floatingLabels = jsonFormat1(FloatingLabels)
  implicit val highchartData: JsonFormat[RootConfig] = CustomJsonFormat(RootConfig)
}