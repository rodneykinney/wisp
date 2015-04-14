/**
 * Created by rodneykinney on 4/13/15.
 */
object Scratch {

  import com.quantifind.charts.Highcharts._

  def main(args: Array[String]): Unit = {
    val x = (0 to 100).map(_ * .01)
    //    line(x.toArray, x.toArray)
//    implicit def arrToI[T](arr: Array[T]) = new PairFromIterable(arr.toIterable)
    line(x)
    line(x.toArray[Double])
    line(x, x)
    line(x, (t: Double) => t * t)
    line((t: Double) => t * t, x)
    histogram(x)
    column(x.map(_.toString), x)
    pie(x.map(_.toString), x)
  }

}
