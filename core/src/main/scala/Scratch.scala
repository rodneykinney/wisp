import java.awt.Color

import allenai._
import allenai.highcharts._

object Scratch {

  val x = (1 to 100).map(_ * .01)

  def main(args: Array[String]): Unit = {
    newAPI
    //    oldAPI
  }

  def newAPI: Unit = {
    import Plot._
    val l = line(x, x).xAxis.axisType(AxisType.logarithmic)
    val a = area(x,x)
    val lineChart = line(x, x)
        .title.text("Two curves")
        .addSeries(x, (t: Double) => math.sin(t * 4 * math.Pi))
        .legend("line", "sinusoid")
        .xAxis.title.text("Distance")
        .yAxis.min(-1)
        .yAxis.max(1)
        .addXAxis(Axis(title = AxisTitle("Other Axis")))
        .yAxis.title.text("Dollars")
        .xAxis.axisType(AxisType.logarithmic)
        .layout.width(800)
    val x1 = x.take(10) ++ x.drop(80).take(10)
    val lc1 = line(x1, x1)
        .xAxis.title.text("Something")
        .xAxis.min(.3)
        .xAxis.max(.7)
        .yAxis.min(-1)
        .yAxis.max(1)
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

  def oldAPI: Unit = {
    import com.quantifind.charts.Highcharts._
    //    val chart = line(List(10.0, 20.0, 30.0))
    //    xAxisCategories("ten", "twenty", "thirty")
    //    xAxisType(com.quantifind.charts.highcharts.AxisType.logarithmic)
    //    chart.legend(List("linear2", "sin2"))
    line(x, (t: Double) => math.sin(t * 4 * math.Pi))
    line(x, (t: Double) => math.sin(t * 4 * math.Pi))
    //    legend(List("linear", "sin"))
  }

  def testSyntax: Unit = {
    import com.quantifind.charts.Highcharts._
    line(x)
    line(x, x)
    line(x, (t: Double) => t * t)
    line((t: Double) => t * t, x)
    histogram(x)
    column(x.map(_.toString), x)
    pie(x.map(_.toString), x)

    val a = x.toArray
    line(a)
    line(a, a)
    line(a, (t: Double) => t * t)
    line((t: Double) => t * t, a)
    //    histogram(a)
    column(a.map(_.toString), a)
    pie(a.map(_.toString), a)
  }

}
