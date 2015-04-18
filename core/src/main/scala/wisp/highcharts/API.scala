package wisp.highcharts

import spray.json.JsValue

import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
trait API {
  def help = {
    val methodDescriptions = for {
      m <- this.getClass.getDeclaredMethods
      method = m.getName
      if (method.indexOf('$') < 0 && method != "help")
    } yield {
      val msg = Option(m.getAnnotation(classOf[WebMethod])).map(s => s" -- ${s.action}").getOrElse("")
      val params = m.getParameterTypes.map(_.getSimpleName) match {
        case Array() => ""
        case a => a.mkString("(", ", ", ")")
      }
      s"${m.getName}$params$msg"
    }
    println("")
    for (line <- methodDescriptions.filterNot(_.startsWith("other(")).sorted) {
      println(line)
    }
    for (line <- methodDescriptions.filter(_.startsWith("other(")).sorted) {
      println(line)
    }
  }

  def other(name: String, value: JsValue): Any
}
