package com.quantifind.charts.repl

import allenai.highcharts.{YValue, XYValue, RichPoint, Point}

import scala.language.implicitConversions

/**
 * User: austin
 * Date: 12/2/14
 *
 * Uses the magnet pattern to resolve that Iterable with PartialFunction should be treated like
 * Iterable instead of PartialFunction, since I want a method with the same name + type signature
 * to work on both Iterable and Functions
 *
 **/
trait IterablePair[A, B] {
  def toIterables: (Iterable[A], Iterable[B])
}

trait SeriesData {
  def points: Seq[Point]
}

trait HasCategories {
  def getCategories: Iterable[String]
}

trait IterablePairConversions {
  implicit class PairFromStringAndIterable[B](ab: (Iterable[String], Iterable[B]))
      extends IterablePair[String, B] with HasCategories {
    def toIterables: (Iterable[String], Iterable[B]) = (ab._1, ab._2)

    def getCategories: Iterable[String] = ab._1
  }

  implicit class DataFromStringAndIterable[B <% Double](ab: (Iterable[B], Iterable[String]))
      extends SeriesData {
    def points = ab._2.zip(ab._1).map { case (l, y) => RichPoint(y = Some(y.toDouble), x = None, name = l) }.toSeq
  }

  implicit class DataFromStringAndArray[B <% Double](ab: (Iterable[B], Array[String]))
      extends SeriesData {
    def points = ab._2.zip(ab._1).map { case (l, y) => RichPoint(y = Some(y.toDouble), x = None, name = l) }.toSeq
  }

  implicit class PairFromStringAndArray[B](ab: (Array[String], Array[B]))
      extends IterablePair[String, B] with HasCategories {
    def toIterables: (Iterable[String], Iterable[B]) = (ab._1, ab._2)

    def getCategories: Iterable[String] = ab._1
  }

  implicit class PairFromIterableStringTuple[B](ab: (Iterable[(String, B)]))
      extends IterablePair[String, B] with HasCategories {
    def toIterables: (Iterable[String], Iterable[B]) = (ab.map(_._1), ab.map(_._2))

    def getCategories: Iterable[String] = ab.map(_._1)
  }

  implicit class DataFromIterableStringTuple[B <% Double](ab: (Iterable[(String, B)]))
      extends SeriesData {
    def points = ab.map { case (l, y) => RichPoint(name = l, y = Some(y.toDouble), x = None) }.toSeq
  }
  implicit class DataFromArrayStringTuple[B <% Double](ab: (Array[(String, B)]))
      extends SeriesData {
    def points = ab.map { case (l, y) => RichPoint(name = l, y = Some(y.toDouble), x = None) }.toSeq
  }

  implicit class PairFromArrayStringTuple[B](ab: (Array[(String, B)]))
      extends IterablePair[String, B] with HasCategories {
    def toIterables: (Iterable[String], Iterable[B]) = (ab.map(_._1), ab.map(_._2))

    def getCategories: Iterable[String] = ab.map(_._1)
  }

  implicit class PairFromIterableFunction[A, B](ab: (Iterable[A], A => B))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._1.map(ab._2))
  }

  implicit class DataFromIterableFunction[A <% Double, B <% Double](ab: (Iterable[A], A => B))
      extends SeriesData {
    def points = ab._1.map(x => XYValue(x.toDouble, ab._2(x).toDouble)).toSeq
  }

  implicit class PairFromArrayFunction[A, B](ab: (Array[A], A => B))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._1.map(ab._2))
  }

  implicit class DataFromArrayFunction[A <% Double, B <% Double](ab: (Array[A], A => B))
      extends SeriesData {
    def points = ab._1.map(x => XYValue(x.toDouble, ab._2(x).toDouble)).toSeq
  }

  implicit class PairFromFunctionArray[A, B](ab: (A => B, Array[A]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._2, ab._2.map(ab._1))
  }

  implicit class DataFromFunctionArray[A <% Double, B <% Double](ab: (A => B, Array[A]))
      extends SeriesData {
    def points = ab._2.map(x => XYValue(x.toDouble, ab._1(x).toDouble)).toSeq
  }

  implicit class PairFromFunction[A, B](ab: (A => B, Iterable[A]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._2, ab._2.map(ab._1))
  }

  implicit class DataFromFunctionIterable[A <% Double, B <% Double](ab: (A => B, Iterable[A]))
      extends SeriesData {
    def points = ab._2.map(x => XYValue(x.toDouble, ab._1(x).toDouble)).toSeq
  }

  implicit class PairFromPairOfIterables[A, B](ab: (Iterable[A], Iterable[B]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._2)
  }

  implicit class DataFromPairOfIterables[A <% Double, B <% Double](ab: (Iterable[A], Iterable[B]))
      extends SeriesData {
    def points = ab._1.zip(ab._2).map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class PairFromPairOfArrays[A, B](ab: (Array[A], Array[B]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._2)
  }

  implicit class DataFromPairOfArrays[A <% Double, B <% Double](ab: (Array[A], Array[B]))
      extends SeriesData {
    def points = ab._1.zip(ab._2).map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class PairFromIterableTuple[A, B](ab: (Iterable[(A, B)]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab.map(_._1), ab.map(_._2))
  }

  implicit class DataFromIterableTuple[A <% Double, B <% Double](ab: (Iterable[(A, B)]))
      extends SeriesData {
    def points = ab.map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class PairFromArrayTuple[A, B](ab: (Array[(A, B)]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab.map(_._1), ab.map(_._2))
  }

  implicit class DataFromArrayTuple[A <% Double, B <% Double](ab: (Array[(A, B)]))
      extends SeriesData {
    def points = ab.map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class PairFromIterable[B](b: (Iterable[B]))
      extends IterablePair[Int, B] {
    def toIterables = ((0 until b.size), b)
  }

  implicit class DataFromIterable[B <% Double](b: (Iterable[B]))
      extends SeriesData {
    def points = b.map(y => YValue(y.toDouble)).toSeq
  }

  implicit class PairFromArray[B](b: (Array[B]))
      extends IterablePair[Int, B] {
    def toIterables = ((0 until b.size), b)
  }

  implicit class DataFromArray[B <% Double](b: (Array[B]))
      extends SeriesData {
    def points = b.map(y => YValue(y.toDouble)).toSeq
  }

}