package wisp.highcharts

import scala.language.implicitConversions

trait SeriesData {
  def points: Seq[Point]

  def categories =
    if (points.forall(_.isInstanceOf[RichPoint]))
      Some(points.map(_.Name.get))
    else
      None
}

trait SeriesDataConversions {

  implicit class DataFromStringAndIterable[B <% Double](ab: (Iterable[B], Iterable[String]))
    extends SeriesData {
    def points = ab._2.zip(ab._1).map { case (l, y) => RichPoint(y = Some(y.toDouble), x = None, name = l)}.toSeq
  }

  implicit class DataFromStringAndArray[B <% Double](ab: (Iterable[B], Array[String]))
    extends SeriesData {
    def points = ab._2.zip(ab._1).map { case (l, y) => RichPoint(y = Some(y.toDouble), x = None, name = l)}.toSeq
  }

  implicit class DataFromIterableStringTuple[B <% Double](ab: (Iterable[(String, B)]))
    extends SeriesData {
    def points = ab.map { case (l, y) => RichPoint(name = l, y = Some(y.toDouble), x = None)}.toSeq
  }

  implicit class DataFromArrayStringTuple[B <% Double](ab: (Array[(String, B)]))
    extends SeriesData {
    def points = ab.map { case (l, y) => RichPoint(name = l, y = Some(y.toDouble), x = None)}.toSeq
  }

  implicit class DataFromIterableFunction[A <% Double, B <% Double](ab: (Iterable[A], A => B))
    extends SeriesData {
    def points = ab._1.map(x => XYValue(x.toDouble, ab._2(x).toDouble)).toSeq
  }

  implicit class DataFromArrayFunction[A <% Double, B <% Double](ab: (Array[A], A => B))
    extends SeriesData {
    def points = ab._1.map(x => XYValue(x.toDouble, ab._2(x).toDouble)).toSeq
  }

  implicit class DataFromFunctionArray[A <% Double, B <% Double](ab: (A => B, Array[A]))
    extends SeriesData {
    def points = ab._2.map(x => XYValue(x.toDouble, ab._1(x).toDouble)).toSeq
  }

  implicit class DataFromFunctionIterable[A <% Double, B <% Double](ab: (A => B, Iterable[A]))
    extends SeriesData {
    def points = ab._2.map(x => XYValue(x.toDouble, ab._1(x).toDouble)).toSeq
  }

  implicit class DataFromPairOfIterables[A <% Double, B <% Double](ab: (Iterable[A], Iterable[B]))
    extends SeriesData {
    def points = ab._1.zip(ab._2).map { case (x, y) => XYValue(x.toDouble, y.toDouble)}.toSeq
  }

  implicit class DataFromPairOfArrays[A <% Double, B <% Double](ab: (Array[A], Array[B]))
    extends SeriesData {
    def points = ab._1.zip(ab._2).map { case (x, y) => XYValue(x.toDouble, y.toDouble)}.toSeq
  }

  implicit class DataFromIterableTuple[A <% Double, B <% Double](ab: (Iterable[(A, B)]))
    extends SeriesData {
    def points = ab.map { case (x, y) => XYValue(x.toDouble, y.toDouble)}.toSeq
  }

  implicit class DataFromArrayTuple[A <% Double, B <% Double](ab: (Array[(A, B)]))
    extends SeriesData {
    def points = ab.map { case (x, y) => XYValue(x.toDouble, y.toDouble)}.toSeq
  }

  implicit class DataFromIterable[B <% Double](b: (Iterable[B]))
    extends SeriesData {
    def points = b.map(y => YValue(y.toDouble)).toSeq
  }

  implicit class DataFromArray[B <% Double](b: (Array[B]))
    extends SeriesData {
    def points = b.map(y => YValue(y.toDouble)).toSeq
  }

  implicit class DataFromStringIterable(s: Iterable[String])
    extends SeriesData {
    def points = s.zipWithIndex.map { case (n, i) => RichPoint(x = None, y = None, name = n)}.toSeq
  }

  implicit class DataFromStringArray(s: Array[String])
    extends SeriesData {
    def points = s.zipWithIndex.map { case (n, i) => RichPoint(x = None, y = None, name = n)}
  }


}