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
trait SeriesData {
  def points: Seq[Point]
}

trait IterablePairConversions {
  implicit class DataFromStringAndIterable[B <% Double](ab: (Iterable[B], Iterable[String]))
      extends SeriesData {
    def points = ab._2.zip(ab._1).map { case (l, y) => RichPoint(y = Some(y.toDouble), x = None, name = l) }.toSeq
  }

  implicit class DataFromStringAndArray[B <% Double](ab: (Iterable[B], Array[String]))
      extends SeriesData {
    def points = ab._2.zip(ab._1).map { case (l, y) => RichPoint(y = Some(y.toDouble), x = None, name = l) }.toSeq
  }

  implicit class DataFromIterableStringTuple[B <% Double](ab: (Iterable[(String, B)]))
      extends SeriesData {
    def points = ab.map { case (l, y) => RichPoint(name = l, y = Some(y.toDouble), x = None) }.toSeq
  }
  implicit class DataFromArrayStringTuple[B <% Double](ab: (Array[(String, B)]))
      extends SeriesData {
    def points = ab.map { case (l, y) => RichPoint(name = l, y = Some(y.toDouble), x = None) }.toSeq
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
    def points = ab._1.zip(ab._2).map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class DataFromPairOfArrays[A <% Double, B <% Double](ab: (Array[A], Array[B]))
      extends SeriesData {
    def points = ab._1.zip(ab._2).map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class DataFromIterableTuple[A <% Double, B <% Double](ab: (Iterable[(A, B)]))
      extends SeriesData {
    def points = ab.map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class DataFromArrayTuple[A <% Double, B <% Double](ab: (Array[(A, B)]))
      extends SeriesData {
    def points = ab.map { case (x, y) => XYValue(x.toDouble, y.toDouble) }.toSeq
  }

  implicit class DataFromIterable[B <% Double](b: (Iterable[B]))
      extends SeriesData {
    def points = b.map(y => YValue(y.toDouble)).toSeq
  }

  implicit class DataFromArray[B <% Double](b: (Array[B]))
      extends SeriesData {
    def points = b.map(y => YValue(y.toDouble)).toSeq
  }

}