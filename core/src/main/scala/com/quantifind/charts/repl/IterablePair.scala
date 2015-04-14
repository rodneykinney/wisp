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

//class IterableIterable[A, B](a: Iterable[A], b: Iterable[B]) extends IterablePair[A, B] {
//  def toIterables: (Iterable[A], Iterable[B]) = (a, b)
//}

//class IterableFunction[A, B](a: Iterable[A], b: A => B) extends IterablePair[A, B] {
//  def toIterables: (Iterable[A], Iterable[B]) = (a, a.map(b))
//}

//class FunctionIterable[A, B](a: B => A, b: Iterable[B]) extends IterablePair[A, B] {
//  def toIterables: (Iterable[A], Iterable[B]) = (b.map(a), b)
//}

//class StringIterableIterable[B](val a: Iterable[String], b: Iterable[B]) extends IterablePair[Int, B]  {
//  def toIterables: (Iterable[Int], Iterable[B]) = ((0 until a.size), b)
//  def getCategories: Iterable[String] = a
//}

trait IterablePairLowerPriorityImplicits {
  //  implicit def mkIterableFunction[A, B](ab: (Iterable[A], A => B)): IterablePair[Iterable[A], A => B, A, B] = new IterableFunction(ab._1, ab._2)
  //  implicit def mkFunctionIterable[A, B](ab: (B => A, Iterable[B])): IterablePair[B => A, Iterable[B], A, B] = new FunctionIterable(ab._1, ab._2)
}

object IterablePair extends IterablePairLowerPriorityImplicits {
  //  implicit def mkStringIterableIterable[B](ab: (Iterable[String], Iterable[B])) = new StringIterableIterable(ab._1, ab._2)
  //  implicit def mkStringIterableIterable[B](ab: (Iterable[(String, B)])) = new StringIterableIterable(ab.map(_._1), ab.map(_._2))
  //
  //  implicit def mkStringArrayArray[B](ab: (Array[String], Array[B])) = new StringIterableIterable(ab._1.toSeq, ab._2.toSeq)
  //  implicit def mkStringArrayArray[B](ab: (Array[(String, B)])) = new StringIterableIterable(ab.map(_._1).toSeq, ab.map(_._2).toSeq)
  //
  //  implicit def mkIterableIterable[A, B](ab: (Iterable[A], Iterable[B])) = new IterableIterable(ab._1, ab._2)
  //  implicit def mkIterableIterable[A, B](ab: (Iterable[(A, B)])) = new IterableIterable(ab.map(_._1), ab.map(_._2))
  //  implicit def mkIterableIterable[B](b: (Iterable[B]), dummy:Unit = ()) = new IterableIterable((0
  //    until b.size), b)
  //
  //  implicit def mkArrayArray[A, B](ab: (Array[A], Array[B])) = new IterableIterable(ab._1.toSeq, ab._2.toSeq)
  //  implicit def mkArrayArray[A, B](ab: (Array[(A, B)])) = new IterableIterable(ab.map(_._1).toSeq, ab.map(_._2).toSeq)
  //  implicit def mkArrayArray[B](b: (Array[B])) = new IterableIterable((0 until b.size), b.toSeq)
  //
  //  implicit def mkArrayIterable[A, B](ab: (Array[A], Iterable[B])) = new IterableIterable(ab._1.toSeq, ab._2)
  //  implicit def mkIterableArray[A, B](ab: (Iterable[A], Array[B])) = new IterableIterable(ab._1, ab._2.toSeq)
}