/**
 * Created by rodneykinney on 4/13/15.
 */
object Scratch {
  import com.quantifind.charts.Highcharts._
  def main(args: Array[String]): Unit = {
    val x = (0 to 100).map(_ * .01)
    line(x)
  }

}
