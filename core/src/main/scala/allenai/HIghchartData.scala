package allenai

import java.awt.Color
import java.lang.reflect.Modifier

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.util.control.NonFatal

/**
 * Created by rodneykinney on 4/15/15.
 */
case class HighchartData(x: Int, y: String) extends HighchartElement {
  def toJson = HighchartElementJsonFormat(HighchartData).write(this)
}

//case class HighchartData(
//    chart: Chart,
//        colors: Seq[Color],
//data: Data,
//exporting: Exporting = ,
//labels: Labels,
//legend: Legend,
//pane: Pane,
//plotOptions: PlotOptions,
//series: Seq[Series],
//subtitle: Subtitle,
//title: Title,
//tooltip: Tooltip,
//xAxis: Axis,
//yAxis: Axis
//    )

trait HighchartElement extends Product {
  val options = collection.mutable.Map[String, JsValue]()
  def addOption[T: JsonFormat](key: String, value: T): Unit = {
    options.update(key, value.toJson)
  }
}


