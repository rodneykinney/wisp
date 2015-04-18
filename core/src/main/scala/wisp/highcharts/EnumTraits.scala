package wisp.highcharts

import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * Created by rodneykinney on 4/17/15.
 */
trait EnumTrait {
  override def toString = this.getClass.getSimpleName.stripSuffix("$")
}

sealed trait HAlign extends EnumTrait

object HAlign {
  val LEFT = left
  val CENTER = center
  val RIGHT = right
  case object left extends HAlign
  case object center extends HAlign
  case object right extends HAlign
}

sealed trait VAlign extends EnumTrait

object VAlign {
  val TOP = top
  val MIDDLE = middle
  val BOTTOM = bottom
  case object top extends VAlign
  case object middle extends VAlign
  case object bottom extends VAlign
}

sealed trait AxisType extends EnumTrait

object AxisType {
  val CATEGORY = category
  val DATETIME = datetime
  val LINEAR = linear
  val LOGARITHMIC = logarithmic
  case object category extends AxisType
  case object datetime extends AxisType
  case object linear extends AxisType
  case object logarithmic extends AxisType
}

sealed trait SeriesType extends EnumTrait

object SeriesType {
  val AREA = area
  val AREASPLINE = areaspline
  val BAR = bar
  val BOXPLOT = boxplot
  val COLUMN = column
  val LINE = line
  val PIE = pie
  val SCATTER = scatter
  val SPLINE = spline
  case object area extends SeriesType
  case object areaspline extends SeriesType
  case object bar extends SeriesType
  case object boxplot extends SeriesType
  case object column extends SeriesType
  case object line extends SeriesType
  case object pie extends SeriesType
  case object scatter extends SeriesType
  case object spline extends SeriesType
}

sealed trait Orientation extends EnumTrait

object Orientation {
  case object vertical extends Orientation
  case object horizontal extends Orientation
}

sealed trait Stacking extends EnumTrait

object Stacking {
  val NORMAL = normal
  val PERCENT = percent
  case object normal extends Stacking
  case object percent extends Stacking
}


