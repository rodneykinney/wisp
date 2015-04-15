package allenai

import com.quantifind.charts.WebPlotter
import com.quantifind.charts.highcharts.{AxisType, LinePlot, Highchart, SeriesType}
import com.quantifind.charts.repl._

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends WebPlotter with IterablePairConversions with ContainerConversions {
  implicit val plotter = this

  def line(xy: XYData) =
    new LinePlot(Highchart(xy.x, xy.y, SeriesType.line), plotter)

  def lineCategoricalY(xy: LabeledXData) = {
    val (x, labels) = (xy.x, xy.labels)
    val hc = Highchart(x, (0 until x.size), SeriesType.line)
        .updateYAxis(_.copy(axisType = AxisType.category.toString,
      categories = Some(labels.toArray)))
    new LinePlot(hc, plotter)
  }

  def lineCategoricalX(xy: LabeledYData) = {
    val (y, labels) = (xy.y, xy.labels)
    val hc = Highchart(y, (0 until y.size), SeriesType.line)
        .updateXAxis(_.copy(axisType = AxisType.category.toString,
      categories = Some(labels.toArray)))
    new LinePlot(hc, plotter)
  }

  def lineCategoricalXY(xy: LabeledXYData) = {
    val (x, y) = (xy.xLabels, xy.yLabels)
    val hc = Highchart((0 until x.size), (0 until y.size), SeriesType.line)
        .updateXAxis(_.copy(axisType = AxisType.category.toString,
      categories = Some(x.asInstanceOf[Iterable[String]].toArray)))
        .updateYAxis(_.copy(axisType = AxisType.category.toString,
      categories = Some(y.asInstanceOf[Iterable[String]].toArray)))
    new LinePlot(hc, plotter)
  }
}

trait ContainerConversions {
  // value -> Some(value)
  implicit def optionWrap[T](value: T): Option[T] = Option(value)

}
