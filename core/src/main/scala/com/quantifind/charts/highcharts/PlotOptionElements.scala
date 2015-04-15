package com.quantifind.charts.highcharts

/**
 * User: austin
 * Date: 12/12/14
 */
object Stacking {
  type Type = String
  val NORMAL = "normal"
  val PERCENT = "percent"
  def values = Set(NORMAL, PERCENT)
  case object normal extends Stacking
  case object percent extends Stacking
}

sealed trait Stacking {
  override def toString = this.getClass.getSimpleName.stripSuffix("$")
}
