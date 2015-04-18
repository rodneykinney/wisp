package wisp.highcharts

import java.awt.Color
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

sealed trait DashStyle extends EnumTrait

object DashStyle {
  val SOLID = Solid
  val SHORT_DASH = ShortDash
  val SHORT_DOT = ShortDot
  val SHORT_DASH_DOT = ShortDashDot
  val SHORT_DASH_DOT_DOT = ShortDashDotDot
  val DOT = Dot
  val DASH = Dash
  val LONG_DASH = LongDash
  val LONG_DASH_DOT_DOT = LongDashDotDot
  val LONG_DASH_DOT = LongDashDot
  val DASH_DOT = DashDot
  case object Solid extends DashStyle
  case object ShortDash extends DashStyle
  case object ShortDot extends DashStyle
  case object ShortDashDot extends DashStyle
  case object ShortDashDotDot extends DashStyle
  case object Dot extends DashStyle
  case object Dash extends DashStyle
  case object LongDash extends DashStyle
  case object LongDashDot extends DashStyle
  case object LongDashDotDot extends DashStyle
  case object DashDot extends DashStyle
}

sealed trait MarkerSymbol extends EnumTrait

object MarkerSymbol {
  val CIRCLE = circle
  val SQUARE = square
  val DIAMOND = diamond
  val TRIANGLE  = triangle
  val TRIANGLE_DOWN = `triangle-down`
  case object circle extends MarkerSymbol
  case object square extends MarkerSymbol
  case object diamond extends MarkerSymbol
  case object triangle extends MarkerSymbol
  case object `triangle-down` extends MarkerSymbol
}


