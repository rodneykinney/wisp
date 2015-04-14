/**
 * Created by rodneykinney on 4/13/15.
 */
object Scratch {

  import com.quantifind.charts.Highcharts._

  def main(args: Array[String]): Unit = {
    val x = (0 to 100).map(_ * .01)
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
