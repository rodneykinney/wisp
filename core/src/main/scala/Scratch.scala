import java.awt.Color

import wisp._
import wisp.highcharts._

object Scratch {

  val x = (1 to 100).map(_ * .01)
  val y = x.map(t => t * t * t * t)

  def main(args: Array[String]): Unit = {
    //    undoRedo()
    suite()
  }

  def undoRedo(): Unit = {
    import Plot._
    val lc = line(1 to 10)
    lc.title.text("Title")
    undo()
    redo()
  }

  def suite(): Unit = {
    import Plot._
    val b = bar(x.take(10))
      .addSeries(x.drop(20).take(10))
      .defaultSettings.stacked
      .series(0).name("Apples")
      .series(1).name("Oranges")
      .addFloatingLabel(300, 300, "Look at me!")
      .legend.title("Fruits")
    column(x.take(10))
      .addSeries(x.drop(20).take(10))
      .defaultSettings.stacked
    val as = areaSpline(x, y)
    val l = line(x, x).xAxis.axisType(AxisType.logarithmic)
    val a = area(x, x)
    val lineChart = line(x, x)
      .title.text("Two curves")
      .addSeries(x, (t: Double) => math.sin(t * 4 * math.Pi))
      .series(0).name("line")
      .series(1).name("sinusoid")
      .xAxis.title.text("Distance")
      .yAxis.range(-1, 1)
      .addXAxis(Axis(title = AxisTitle("Other Axis")))
      .yAxis.title.text("Dollars")
      .xAxis.axisType(AxisType.logarithmic)
      .layout.size(800, 400)
    val x1 = x.take(10) ++ x.drop(80).take(10)
    val lcNA = line(List(1.0, 2.0, 3.0), List("One", "Two", "Three"))
    .xAxis.categories(List("A","B","C"))
    .yAxis.categories(List("Do","Re","Mi"))
    .series(0).showPointLabels(i => DataLabel(backgroundColor = Color.LIGHT_GRAY))
  }
}
