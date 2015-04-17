import java.awt.Color

import wisp._
import wisp.highcharts._

object Scratch {

  val x = (1 to 100).map(_ * .01)
  val y = x.map(t => t * t * t * t)

  def main(args: Array[String]): Unit = {
    newAPI
    //    oldAPI
  }

  def newAPI: Unit = {
    import Plot._
    val b = bar(x.take(10))
        .addSeries(x.drop(20).take(10))
        .stack()
        .series(0).name("Apples")
        .series(1).name("Oranges")
    column(x.take(10))
        .addSeries(x.drop(20).take(10))
        .stack()
    val as = areaSpline(x, y)
    val l = line(x, x).xAxis.axisType(AxisType.logarithmic)
    val a = area(x, x)
    val lineChart = line(x, x)
        .title.text("Two curves")
        .addSeries(x, (t: Double) => math.sin(t * 4 * math.Pi))
        .legend("line", "sinusoid")
        .xAxis.title.text("Distance")
        .yAxis.range(-1, 1)
        .addXAxis(Axis(title = AxisTitle("Other Axis")))
        .yAxis.title.text("Dollars")
        .xAxis.axisType(AxisType.logarithmic)
        .layout.size(800, 400)
    val x1 = x.take(10) ++ x.drop(80).take(10)
    val lc1 = line(x1, x1)
        .xAxis.title.text("Something")
        .xAxis.range(.3, .7)
        .yAxis.range(-1, 1)
        .exporting.enabled(false)

    //    val lcAA = line(List("a", "b", "c"), List("One", "Two", "Three"))
    //    val lcAN = line(List("a", "b", "c"), List(1.0, 2.0, 3.0))
    val lcNA = line(List(1.0, 2.0, 3.0), List("One", "Two", "Three"))
    //    val lc2 = line(x, x)
    //        .title("Categories")
    //        .xAxisCategories(x.map(t => s"$t.$t"))
    //    val pieChart = ???
    //    line(x, (t: Double) => math.sin(t * 4 * math.Pi)).title("Sin wave")
  }
}
