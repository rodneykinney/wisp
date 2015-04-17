package wisp.highcharts

import wisp.HtmlPlotter

import scala.util.Random
import spray.json._
import HighchartsJson._

/**
 * Created by rodneykinney on 4/16/15.
 */
class HighchartsHtmlPlotter extends HtmlPlotter[Highchart, Highchart] {
  def idFor(plot: Highchart) = plot
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

}
