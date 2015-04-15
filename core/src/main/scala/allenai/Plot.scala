package allenai

import com.quantifind.charts.WebPlotter
import com.quantifind.charts.highcharts.{LinePlot, Highchart, SeriesType}
import com.quantifind.charts.repl.{IterablePair, IterablePairConversions}

/**
 * Created by rodneykinney on 4/14/15.
 */
object Plot extends WebPlotter with IterablePairConversions with ContainerConversions {
  implicit val plotter = this

  def line[C, D](xy: IterablePair[C, D]) = {
    xy.toIterables match {
      case (x, y) if x.forall(_.isInstanceOf[String]) && y.forall(_.isInstanceOf[String]) =>
        new LinePlot(Highchart((0 until x.size), (0 until y.size), SeriesType.line), plotter)
            .xAxisCategories(x.asInstanceOf[Iterable[String]])
            .yAxisCategories(y.asInstanceOf[Iterable[String]])
      case (x, y) if x.forall(_.isInstanceOf[String]) =>
        new LinePlot(Highchart((0 until x.size), y, SeriesType.line), plotter)
            .xAxisCategories(x.asInstanceOf[Iterable[String]])
      case (x, y) if y.forall(_.isInstanceOf[String]) =>
        new LinePlot(Highchart(x, (0 until y.size), SeriesType.line), plotter)
            .yAxisCategories(y.asInstanceOf[Iterable[String]])
      case (x, y) =>
        new LinePlot(Highchart(x, y, SeriesType.line), plotter)
    }
  }
}

trait ContainerConversions {
  // value -> Some(value)
  implicit def optionWrap[T](value: T): Option[T] = Option(value)

}
