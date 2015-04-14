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