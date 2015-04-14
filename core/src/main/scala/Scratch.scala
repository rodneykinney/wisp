import com.quantifind.charts.Highcharts._

object Scratch {

  val x = (0 to 100).map(_ * .01)

  def main(args: Array[String]): Unit = {
    val chart = line(x,x)
    hold()
    line(x, (t: Double) => math.sin(t*4*math.Pi))
    chart.legend(List("linear2", "sin2"))
    legend(List("linear", "sin"))

  }

  def testSyntax: Unit = {
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
