package wisp.highcharts

import spray.json.{JsonWriter, JsValue}
import wisp.CustomJsonObject

import java.awt.Color
import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
case class Legend(
                   x: Option[Int] = None,
                   y: Option[Int] = None,
                   title: LegendTitle = null,
                   enabled: Boolean = true,
                   align: HAlign = null,
                   borderWidth: Int = 2,
                   borderColor: Color = null,
                   backgroundColor: Color = null,
                   borderRadius: Option[Int] = None,
                   floating: Option[Boolean] = None,
                   layout: Orientation = null,
                   shadow: Option[Boolean] = None,
                   verticalAlign: VAlign = null,
                   other: Map[String, JsValue] = Map()
                   ) extends CustomJsonObject {
  def api[T](update: Legend => T) = new LegendAPI(this, update)
}

class LegendAPI[T](legend: Legend, update: Legend => T) extends API {
  @WebMethod(action = "(x,y) position")
  def position(x: Int, y: Int) = update(legend.copy(x = Some(x), y = Some(y)))

  @WebMethod
  def enabled(x: Boolean) = update(legend.copy(enabled = x))

  @WebMethod(action = "Allow legend to overlap plot area")
  def floating(x: Boolean) = update(legend.copy(floating = Some(x)))

  @WebMethod
  def horizontalJustification(x: HAlign) = update(legend.copy(align = x))

  @WebMethod
  def borderColor(x: Color) = update(legend.copy(borderColor = x))

  @WebMethod
  def backgroundColor(x: Color) = update(legend.copy(backgroundColor = x))

  @WebMethod
  def borderRadius(x: Int) = update(legend.copy(borderRadius = Some(x)))

  @WebMethod
  def borderWidth(x: Int) = update(legend.copy(borderWidth = x))

  @WebMethod(action = "horizontal/vertical layout")
  def layout(x: Orientation) = update(legend.copy(layout = x))

  @WebMethod(action = "Show shadow (background color must be set)")
  def shadow(x: Boolean) = update(legend.copy(shadow = Some(x)))

  @WebMethod(action = "Title text and CSS style")
  def title(text: String, style: Map[String, String] = Map()) = update(legend.copy(title = LegendTitle(text, style)))

  @WebMethod
  def verticalJustification(x: VAlign) = update(legend.copy(verticalAlign = x))

  @WebMethod(action = "Add additional values to the JSON object")
  def addOption[V: JsonWriter](name: String, value: V)
  = update(legend.copy(other = legend.other + (name -> implicitly[JsonWriter[V]].write(value))))

}


case class LegendTitle(text: String, style: Map[String, String] = Map())

