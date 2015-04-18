package wisp.highcharts

import wisp.HtmlPlotter

import scala.util.Random
import spray.json._
import HighchartsJson._

/**
 * Created by rodneykinney on 4/16/15.
 */
class HighchartsHtmlPlotter extends HtmlPlotter[Plot, Highchart] {

  def renderPlot(hc: Highchart) = {
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

  override def getPlotState(plot: Plot): Highchart = plot.data

  override def setPlotState(plot: Plot, state: Highchart): Unit = plot.data = state
}
