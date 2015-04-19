package wisp.highcharts

import spray.json.JsValue

import scala.collection.mutable.ListBuffer

import java.lang.reflect.Method
import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
trait API {
  def help = {
    val methodDescriptions = for {
      m <- methods
    } yield {
      val msg = Option(m.getAnnotation(classOf[WebMethod])).
        map(_.action).filter(_.length > 0).map(s => s" -- $s").getOrElse("")
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

  def methods = {
    val methodList = new ListBuffer[Method]
    var c: Class[_] = this.getClass
    while (c.getName.endsWith("API")) {
      for {
        method <- c.getDeclaredMethods
        methodName = method.getName
        if method.getAnnotation(classOf[WebMethod]) != null
      }
        methodList.append(method)
      c = c.getSuperclass
    }
    methodList.toList
  }

  def other(name: String, value: JsValue): Any
}
