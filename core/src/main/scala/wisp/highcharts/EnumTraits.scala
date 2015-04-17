package wisp.highcharts

/**
 * Created by rodneykinney on 4/17/15.
 */
trait EnumTrait {
  override def toString = this.getClass.getSimpleName.stripSuffix("$")
}

sealed trait HAlign extends EnumTrait

object HAlign {
  case object left extends HAlign
  case object center extends HAlign
  case object right extends HAlign
}

sealed trait VAlign extends EnumTrait

object VAlign {
  case object top extends VAlign
  case object middle extends VAlign
  case object bottom extends VAlign
}

sealed trait AxisType extends EnumTrait

object AxisType {
  case object category extends AxisType
  case object datetime extends AxisType
  case object linear extends AxisType
  case object logarithmic extends AxisType
}

sealed trait SeriesType extends EnumTrait

object SeriesType {
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
