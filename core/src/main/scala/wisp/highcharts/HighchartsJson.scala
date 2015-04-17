package wisp.highcharts

import java.awt.Color
import java.lang.reflect.Modifier

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.util.control.NonFatal

/**
 * Created by rodneykinney on 4/15/15.
 */
object HighchartsJson {
    implicit def writerToFormat[T](writer: JsonWriter[T]) = new JsonFormat[T] {
      override def write(obj: T): JsValue = writer.write(obj)

      override def read(json: JsValue): T = ???
    }

    implicit val color =
      new JsonWriter[Color] {
        def write(c: Color) = "#%02x%02x%02x".format(c.getRed, c.getGreen, c.getBlue).toJson
      }
    implicit val chart: JsonFormat[Chart] = this(Chart)
    implicit val align: JsonFormat[Align] = asString[Align]
    implicit val title: JsonFormat[Title] = this(Title)
    implicit val axisTitle: JsonFormat[AxisTitle] = this(AxisTitle)
    implicit val axisType: JsonFormat[AxisType] = asString[AxisType]
    implicit val axis: JsonFormat[Axis] = this(Axis)
    implicit val exporting: JsonFormat[Exporting] = this(Exporting)
    implicit val legend: JsonFormat[Legend] = this(Legend)
    implicit val dataLabels: JsonFormat[DataLabels] = this(DataLabels)
    implicit val richPoint: JsonFormat[RichPoint] = this(RichPoint)
    implicit val stacking: JsonFormat[Stacking] = asString[Stacking]
    implicit val plotSettings: JsonFormat[PlotSettings] = this(PlotSettings)
    implicit val plotOptions: JsonFormat[PlotOptions] = this(PlotOptions)
    implicit val data: JsonFormat[Point] = new JsonWriter[Point] {
      def write(obj: Point) = obj match {
        case n: XYValue => (n.x, n.y).toJson
        case n: YValue => n.value.toJson
        case p: RichPoint => richPoint.write(p)
      }
    }
    implicit val seriesType: JsonFormat[SeriesType] = asString[SeriesType]
    implicit val series: JsonFormat[Series] = this(Series)
    implicit val highchartData: JsonFormat[HighchartAPI] = this(HighchartAPI)


  private[this] type JF[T] = JsonWriter[T] // simple alias for reduced verbosity

  def asString[T] = new JsonWriter[T] {
    def write(data: T) = JsString(data.toString)
  }

  protected def extractFieldNames(classManifest: ClassManifest[_]): Array[String] = {
    val clazz = classManifest.erasure
    try {
      // copy methods have the form copy$default$N(), we need to sort them in order, but must account for the fact
      // that lexical sorting of ...8(), ...9(), ...10() is not correct, so we extract N and sort by N.toInt
      val copyDefaultMethods = clazz.getMethods.filter(_.getName.startsWith("copy$default$")).sortBy(
        _.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt)
      val fields = clazz.getDeclaredFields.filterNot { f =>
        f.getName == "options" ||
            f.getName == "toJson" ||
            f.getName.startsWith("$") || Modifier.isTransient(f.getModifiers) || Modifier.isStatic(f.getModifiers)
      }
      if (copyDefaultMethods.length != fields.length)
        sys.error("Case class " + clazz.getName + " declares additional fields")
      if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
        sys.error("Cannot determine field order of case class " + clazz.getName)
      fields.map(f => ProductFormats.unmangle(f.getName))
    } catch {
      case NonFatal(ex) => throw new RuntimeException("Cannot automatically determine case class field names and order " +
          "for '" + clazz.getName + "', please use the 'jsonFormat' overload with explicit field name specification", ex)
    }
  }


  protected def productElement2Field[T](fieldName: String, p: Product, ix: Int, rest: List[JsField] = Nil)
      (implicit writer: JsonWriter[T]): List[JsField] = {
    val value = p.productElement(ix).asInstanceOf[T]
    writer match {
      case _: OptionFormat[_] if (value == None) => rest
      case _ if value == null => rest
      case _ => (fieldName, writer.write(value)) :: rest
    }
  }
  // Case classes with 1 parameters

  def apply[P1: JF, T <: HighchartElement : ClassManifest](construct: (P1) => T): JsonWriter[T] = {
    val Array(p1) = extractFieldNames(classManifest[T])
    new JsonWriter[T] {
      def write(p: T) = {
        val fields = new collection.mutable.ListBuffer[(String, JsValue)]
        fields.sizeHint(1 * 2)
        fields ++= productElement2Field[P1](p1, p, 0)
        fields ++= p.other
        JsObject(fields: _*)
      }

    }
  }

  // Case classes with 2 parameters

  def apply[P1: JF, P2: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2) => T): JsonWriter[T] = {
    val Array(p1, p2) = extractFieldNames(classManifest[T])
    new JsonWriter[T] {
      def write(p: T) = {
        val fields = new collection.mutable.ListBuffer[(String, JsValue)]
        fields.sizeHint(2 * 3)
        fields ++= productElement2Field[P1](p1, p, 0)
        fields ++= productElement2Field[P2](p2, p, 1)
        fields ++= p.other
        JsObject(fields: _*)
      }

    }
  }


  // Case classes with 3 parameters

  def apply[P1: JF, P2: JF, P3: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3) => T): JsonWriter[T] = {
    val Array(p1, p2, p3) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(3 * 4)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 4 parameters

  def apply[P1: JF, P2: JF, P3: JF, P4: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(4 * 5)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 5 parameters

  def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(5 * 6)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 6 parameters

  def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(6 * 7)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 7 parameters

  def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(7 * 8)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 8 parameters

  def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(8 * 9)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 9 parameters

  def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(9 * 10)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 10 parameters

  def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(10 * 11)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 11 parameters

  def apply1[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(11 * 12)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= productElement2Field[P11](p11, p, 10)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 12 parameters

  def apply2[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(12 * 13)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= productElement2Field[P11](p11, p, 10)
      fields ++= productElement2Field[P12](p12, p, 11)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 13 parameters

  def apply3[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(13 * 14)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= productElement2Field[P11](p11, p, 10)
      fields ++= productElement2Field[P12](p12, p, 11)
      fields ++= productElement2Field[P13](p13, p, 12)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 14 parameters

  def apply4[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(14 * 15)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= productElement2Field[P11](p11, p, 10)
      fields ++= productElement2Field[P12](p12, p, 11)
      fields ++= productElement2Field[P13](p13, p, 12)
      fields ++= productElement2Field[P14](p14, p, 13)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 15 parameters

  def apply5[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(15 * 16)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= productElement2Field[P11](p11, p, 10)
      fields ++= productElement2Field[P12](p12, p, 11)
      fields ++= productElement2Field[P13](p13, p, 12)
      fields ++= productElement2Field[P14](p14, p, 13)
      fields ++= productElement2Field[P15](p15, p, 14)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 16 parameters

  def apply6[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, P16: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(16 * 17)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= productElement2Field[P11](p11, p, 10)
      fields ++= productElement2Field[P12](p12, p, 11)
      fields ++= productElement2Field[P13](p13, p, 12)
      fields ++= productElement2Field[P14](p14, p, 13)
      fields ++= productElement2Field[P15](p15, p, 14)
      fields ++= productElement2Field[P16](p16, p, 15)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 17 parameters

  def apply7[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, P16: JF, P17: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) = extractFieldNames(classManifest[T])
new JsonWriter[T] {
    def write(p: T) = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(17 * 18)
      fields ++= productElement2Field[P1](p1, p, 0)
      fields ++= productElement2Field[P2](p2, p, 1)
      fields ++= productElement2Field[P3](p3, p, 2)
      fields ++= productElement2Field[P4](p4, p, 3)
      fields ++= productElement2Field[P5](p5, p, 4)
      fields ++= productElement2Field[P6](p6, p, 5)
      fields ++= productElement2Field[P7](p7, p, 6)
      fields ++= productElement2Field[P8](p8, p, 7)
      fields ++= productElement2Field[P9](p9, p, 8)
      fields ++= productElement2Field[P10](p10, p, 9)
      fields ++= productElement2Field[P11](p11, p, 10)
      fields ++= productElement2Field[P12](p12, p, 11)
      fields ++= productElement2Field[P13](p13, p, 12)
      fields ++= productElement2Field[P14](p14, p, 13)
      fields ++= productElement2Field[P15](p15, p, 14)
      fields ++= productElement2Field[P16](p16, p, 15)
      fields ++= productElement2Field[P17](p17, p, 16)
      fields ++= p.other
      JsObject(fields: _*)
    }

  }
}


  // Case classes with 18 parameters

  def apply8[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, P16: JF, P17: JF, P18: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T): JsonWriter[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) = extractFieldNames(classManifest[T])
    new JsonWriter[T] {
      def write(p: T) = {
        val fields = new collection.mutable.ListBuffer[(String, JsValue)]
        fields.sizeHint(18 * 19)
        fields ++= productElement2Field[P1](p1, p, 0)
        fields ++= productElement2Field[P2](p2, p, 1)
        fields ++= productElement2Field[P3](p3, p, 2)
        fields ++= productElement2Field[P4](p4, p, 3)
        fields ++= productElement2Field[P5](p5, p, 4)
        fields ++= productElement2Field[P6](p6, p, 5)
        fields ++= productElement2Field[P7](p7, p, 6)
        fields ++= productElement2Field[P8](p8, p, 7)
        fields ++= productElement2Field[P9](p9, p, 8)
        fields ++= productElement2Field[P10](p10, p, 9)
        fields ++= productElement2Field[P11](p11, p, 10)
        fields ++= productElement2Field[P12](p12, p, 11)
        fields ++= productElement2Field[P13](p13, p, 12)
        fields ++= productElement2Field[P14](p14, p, 13)
        fields ++= productElement2Field[P15](p15, p, 14)
        fields ++= productElement2Field[P16](p16, p, 15)
        fields ++= productElement2Field[P17](p17, p, 16)
        fields ++= productElement2Field[P18](p18, p, 17)
        fields ++= p.other
        JsObject(fields: _*)
      }

    }
}


    // Case classes with 19 parameters

    def apply9[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, P16: JF, P17: JF, P18: JF, P19: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T): JsonWriter[T] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) = extractFieldNames(classManifest[T])
      new JsonWriter[T] {
        def write(p: T) = {
          val fields = new collection.mutable.ListBuffer[(String, JsValue)]
          fields.sizeHint(19 * 20)
          fields ++= productElement2Field[P1](p1, p, 0)
          fields ++= productElement2Field[P2](p2, p, 1)
          fields ++= productElement2Field[P3](p3, p, 2)
          fields ++= productElement2Field[P4](p4, p, 3)
          fields ++= productElement2Field[P5](p5, p, 4)
          fields ++= productElement2Field[P6](p6, p, 5)
          fields ++= productElement2Field[P7](p7, p, 6)
          fields ++= productElement2Field[P8](p8, p, 7)
          fields ++= productElement2Field[P9](p9, p, 8)
          fields ++= productElement2Field[P10](p10, p, 9)
          fields ++= productElement2Field[P11](p11, p, 10)
          fields ++= productElement2Field[P12](p12, p, 11)
          fields ++= productElement2Field[P13](p13, p, 12)
          fields ++= productElement2Field[P14](p14, p, 13)
          fields ++= productElement2Field[P15](p15, p, 14)
          fields ++= productElement2Field[P16](p16, p, 15)
          fields ++= productElement2Field[P17](p17, p, 16)
          fields ++= productElement2Field[P18](p18, p, 17)
          fields ++= productElement2Field[P19](p19, p, 18)
          fields ++= p.other
          JsObject(fields: _*)
        }

      }
}


      // Case classes with 20 parameters

      def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, P16: JF, P17: JF, P18: JF, P19: JF, P20: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T): JsonWriter[T] = {
        val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) = extractFieldNames(classManifest[T])
        new JsonWriter[T] {
          def write(p: T) = {
            val fields = new collection.mutable.ListBuffer[(String, JsValue)]
            fields.sizeHint(20 * 21)
            fields ++= productElement2Field[P1](p1, p, 0)
            fields ++= productElement2Field[P2](p2, p, 1)
            fields ++= productElement2Field[P3](p3, p, 2)
            fields ++= productElement2Field[P4](p4, p, 3)
            fields ++= productElement2Field[P5](p5, p, 4)
            fields ++= productElement2Field[P6](p6, p, 5)
            fields ++= productElement2Field[P7](p7, p, 6)
            fields ++= productElement2Field[P8](p8, p, 7)
            fields ++= productElement2Field[P9](p9, p, 8)
            fields ++= productElement2Field[P10](p10, p, 9)
            fields ++= productElement2Field[P11](p11, p, 10)
            fields ++= productElement2Field[P12](p12, p, 11)
            fields ++= productElement2Field[P13](p13, p, 12)
            fields ++= productElement2Field[P14](p14, p, 13)
            fields ++= productElement2Field[P15](p15, p, 14)
            fields ++= productElement2Field[P16](p16, p, 15)
            fields ++= productElement2Field[P17](p17, p, 16)
            fields ++= productElement2Field[P18](p18, p, 17)
            fields ++= productElement2Field[P19](p19, p, 18)
            fields ++= productElement2Field[P20](p20, p, 19)
            fields ++= p.other
            JsObject(fields: _*)
          }

        }
}


        // Case classes with 21 parameters

        def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, P16: JF, P17: JF, P18: JF, P19: JF, P20: JF, P21: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T): JsonWriter[T] = {
          val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) = extractFieldNames(classManifest[T])
          new JsonWriter[T] {
            def write(p: T) = {
              val fields = new collection.mutable.ListBuffer[(String, JsValue)]
              fields.sizeHint(21 * 22)
              fields ++= productElement2Field[P1](p1, p, 0)
              fields ++= productElement2Field[P2](p2, p, 1)
              fields ++= productElement2Field[P3](p3, p, 2)
              fields ++= productElement2Field[P4](p4, p, 3)
              fields ++= productElement2Field[P5](p5, p, 4)
              fields ++= productElement2Field[P6](p6, p, 5)
              fields ++= productElement2Field[P7](p7, p, 6)
              fields ++= productElement2Field[P8](p8, p, 7)
              fields ++= productElement2Field[P9](p9, p, 8)
              fields ++= productElement2Field[P10](p10, p, 9)
              fields ++= productElement2Field[P11](p11, p, 10)
              fields ++= productElement2Field[P12](p12, p, 11)
              fields ++= productElement2Field[P13](p13, p, 12)
              fields ++= productElement2Field[P14](p14, p, 13)
              fields ++= productElement2Field[P15](p15, p, 14)
              fields ++= productElement2Field[P16](p16, p, 15)
              fields ++= productElement2Field[P17](p17, p, 16)
              fields ++= productElement2Field[P18](p18, p, 17)
              fields ++= productElement2Field[P19](p19, p, 18)
              fields ++= productElement2Field[P20](p20, p, 19)
              fields ++= productElement2Field[P21](p21, p, 20)
              fields ++= p.other
              JsObject(fields: _*)
            }

          }
}


          // Case classes with 22 parameters

          def apply[P1: JF, P2: JF, P3: JF, P4: JF, P5: JF, P6: JF, P7: JF, P8: JF, P9: JF, P10: JF, P11: JF, P12: JF, P13: JF, P14: JF, P15: JF, P16: JF, P17: JF, P18: JF, P19: JF, P20: JF, P21: JF, P22: JF, T <: HighchartElement : ClassManifest](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T): JsonWriter[T] = {
            val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22) = extractFieldNames(classManifest[T])
            new JsonWriter[T] {
              def write(p: T) = {
                val fields = new collection.mutable.ListBuffer[(String, JsValue)]
                fields.sizeHint(22 * 23)
                fields ++= productElement2Field[P1](p1, p, 0)
                fields ++= productElement2Field[P2](p2, p, 1)
                fields ++= productElement2Field[P3](p3, p, 2)
                fields ++= productElement2Field[P4](p4, p, 3)
                fields ++= productElement2Field[P5](p5, p, 4)
                fields ++= productElement2Field[P6](p6, p, 5)
                fields ++= productElement2Field[P7](p7, p, 6)
                fields ++= productElement2Field[P8](p8, p, 7)
                fields ++= productElement2Field[P9](p9, p, 8)
                fields ++= productElement2Field[P10](p10, p, 9)
                fields ++= productElement2Field[P11](p11, p, 10)
                fields ++= productElement2Field[P12](p12, p, 11)
                fields ++= productElement2Field[P13](p13, p, 12)
                fields ++= productElement2Field[P14](p14, p, 13)
                fields ++= productElement2Field[P15](p15, p, 14)
                fields ++= productElement2Field[P16](p16, p, 15)
                fields ++= productElement2Field[P17](p17, p, 16)
                fields ++= productElement2Field[P18](p18, p, 17)
                fields ++= productElement2Field[P19](p19, p, 18)
                fields ++= productElement2Field[P20](p20, p, 19)
                fields ++= productElement2Field[P21](p21, p, 20)
                fields ++= productElement2Field[P22](p22, p, 21)
                fields ++= p.other
                JsObject(fields: _*)
              }

            }
}

            object ProductFormats {
              private val operators = Map(
                "$eq" -> "=",
                "$greater" -> ">",
                "$less" -> "<",
                "$plus" -> "+",
                "$minus" -> "-",
                "$times" -> "*",
                "$div" -> "/",
                "$bang" -> "!",
                "$at" -> "@",
                "$hash" -> "#",
                "$percent" -> "%",
                "$up" -> "^",
                "$amp" -> "&",
                "$tilde" -> "~",
                "$qmark" -> "?",
                "$bar" -> "|")

              def unmangle(name: String) = operators.foldLeft(name) { case (n, (mangled, unmangled)) =>
                if (n.indexOf(mangled) >= 0) n.replace(mangled, unmangled) else n
              }
            }


          }
