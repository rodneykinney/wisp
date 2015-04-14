package com.quantifind.charts.repl

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

trait HasCategories {
  def getCategories: Iterable[String]
}

trait IterablePairConversions {
  implicit class PairFromStringAndIterable[B](ab: (Iterable[String], Iterable[B]))
      extends IterablePair[String, B] with HasCategories {
    def toIterables: (Iterable[String], Iterable[B]) = (ab._1, ab._2)

    def getCategories: Iterable[String] = ab._1
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

  implicit class PairFromArrayStringTuple[B](ab: (Array[(String, B)]))
      extends IterablePair[String, B] with HasCategories {
    def toIterables: (Iterable[String], Iterable[B]) = (ab.map(_._1), ab.map(_._2))

    def getCategories: Iterable[String] = ab.map(_._1)
  }

  implicit class PairFromIterableFunction[A, B](ab: (Iterable[A], A => B))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._1.map(ab._2))
  }

  implicit class PairFromArrayFunction[A, B](ab: (Array[A], A => B))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._1.map(ab._2))
  }

  implicit class PairFromFunctionArray[A, B](ab: (A => B, Array[A]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._2, ab._2.map(ab._1))
  }

  implicit class PairFromFunction[A, B](ab: (A => B, Iterable[A]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._2, ab._2.map(ab._1))
  }

  implicit class PairFromPairOfIterables[A, B](ab: (Iterable[A], Iterable[B]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._2)
  }

  implicit class PairFromPairOfArrays[A, B](ab: (Array[A], Array[B]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab._1, ab._2)
  }

  implicit class PairFromIterableTuple[A, B](ab: (Iterable[(A, B)]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab.map(_._1), ab.map(_._2))
  }

  implicit class PairFromArrayTuple[A, B](ab: (Array[(A, B)]))
      extends IterablePair[A, B] {
    def toIterables: (Iterable[A], Iterable[B]) = (ab.map(_._1), ab.map(_._2))
  }

  implicit class PairFromIterable[B](b: (Iterable[B]))
      extends IterablePair[Int, B] {
    def toIterables = ((0 until b.size), b)
  }

  implicit class PairFromArray[B](b: (Array[B]))
      extends IterablePair[Int, B] {
    def toIterables = ((0 until b.size), b)
  }


}