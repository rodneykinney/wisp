package wisp.highcharts

import wisp.ChartDisplay
import spray.json._
import DefaultJsonProtocol._

import javax.jws.WebMethod

/**
 * Created by rodneykinney on 4/18/15.
 */
class HistogramAPI(var config: RootConfig,
                   val display: ChartDisplay[RootChart, Int],
                   numBins: Int) extends RootAPI[HistogramAPI] {
  require(config.series.size == 1, "Can only compute histogram from a single series")
  private val originalData =
    for (p <- config.series(0).data) yield (p.X, p.Y) match {
      case (Some(x), None) => x
      case _ => sys.error("Single-value series required")
    }
  update(config.copy(series =
    Vector(Series(Histogram.bin(originalData, numBins).points, `type` = SeriesType.column))))
  this.other("plotOptions",
    Map("column" ->
      Map("pointPadding" -> 0.toJson,
        "borderWidth" -> 0.toJson,
        "groupPadding" -> 0.toJson,
        "shadow" -> false.toJson
      ).toJson
    ).toJson)

  @WebMethod(action = "Number of histogram bins")
  def bins(numBins: Int) = {
    val binnedData: SeriesData = Histogram.bin(originalData, numBins)
    update(config.copy(series = Vector(Series(binnedData.points, `type` = SeriesType.column))))
  }
}

object Histogram extends SeriesDataConversions {
  def bin(data: Seq[Double], numBins: Int) = {
    val nBins = math.min(numBins, data.size)
    var min = data.min
    var max = data.max
    var binSize = (max - min) / nBins
    min = min -.5 * binSize
    max = max +.5 * binSize
    binSize = (max - min) / nBins
    def getBin(x: Double) = Math.floor((x - min) / binSize).toInt
    val binCount = data.map(getBin).groupBy(x => x).mapValues(_.size).toMap
    def binCenter(i: Int) = min + (i + 0.5) * binSize
    val binnedData: SeriesData =
      for (i <- 0 until nBins) yield (binCenter(i), binCount.getOrElse(i, 0))
    binnedData
  }
}

