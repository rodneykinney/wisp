package wisp.highcharts

import wisp.HtmlChartDisplay

import scala.util.Random
import spray.json._
import HighchartsJson._

/**
 * Created by rodneykinney on 4/16/15.
 */
class HighchartsHtmlDisplay extends HtmlChartDisplay[RootChart, RootConfig] {

  def renderChart(hc: RootConfig) = {
    val json = hc.toJson.toString
    val containerId = json.hashCode.toHexString
    s"""
          | <div id="container$containerId"></div>
          |    <script type="text/javascript">
          |        $$(function() {
          |            $$('#container$containerId').highcharts(
          |                $json
          |            );
          |        });
          |    </script>
          |
        """.stripMargin
  }

  override def getChartConfig(plot: RootChart): RootConfig = plot.config

  override def setChartConfig(plot: RootChart, state: RootConfig): Unit = plot.config = state
}
