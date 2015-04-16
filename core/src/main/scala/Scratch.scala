import allenai._
import com.quantifind.charts.highcharts.{AxisType, Stacking}

object Scratch {

  val x = (1 to 100).map(_ * .01)

  def main(args: Array[String]): Unit = {
    import spray.json.DefaultJsonProtocol._
    import spray.json._
    import allenai.AllFormats._
    val d = HighchartAPI(series = List(Series(List(NumericalData(1,10), NumericalData(2,40), NumericalData(3,90)))))
    d.addOption("a","A")
    d.addOption("pair",(3,List("X","Y")))
    d.addOption("empty",List.empty[Int])
    val d2 = HighchartAPI()
    d.addOption("child",d2)
    val s = d.toJson.toString()
    val ss = HighchartAPI(series = List(Series(List(NumericalData(1,10), NumericalData(2,40), NumericalData(3,90))))).toJson.toString()
    newAPI
    //        oldAPI
  }

  def newAPI: Unit = {
    import Plot._
    val l = line(x,x).xAxisType(AxisType.logarithmic)
    val lineChart = line(x, x)
        .title("Two curves")
        .addSeries(x, (t: Double) => math.sin(t * 4 * math.Pi))
        .legend("line", "sinusoid")
        .xAxisLabel("Distance")
        .yAxisLabel("Dollars")
        .xAxisType(AxisType.logarithmic)
    val x1 = x.take(10) ++ x.drop(80).take(10)
    val lc1 = line((x1, x1))
        .xAxisRange(.3, .7)
        .yAxisRange(-1, 1)

    val lcAA = line(List("a", "b", "c"), List("One", "Two", "Three"))
    val lcAN = line(List("a", "b", "c"), List(1.0, 2.0, 3.0))
    val lcNA = line(List(1.0, 2.0, 3.0), List("One", "Two", "Three"))
    //    val lc2 = line(x, x)
    //        .title("Categories")
    //        .xAxisCategories(x.map(t => s"$t.$t"))
    //    val pieChart = ???
    //    line(x, (t: Double) => math.sin(t * 4 * math.Pi)).title("Sin wave")
  }

  def oldAPI: Unit = {
    import com.quantifind.charts.Highcharts._
    val chart = line(List(10.0, 20.0, 30.0))
    xAxisCategories("ten", "twenty", "thirty")
    xAxisType(AxisType.logarithmic)
    //    hold()
    //    line(x, (t: Double) => math.sin(t * 4 * math.Pi))
    //    chart.legend(List("linear2", "sin2"))
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
