package wisp.highcharts

import wisp.WebPlotter

import scala.util.Random
import spray.json._
import HighchartsJson._

/**
 * Created by rodneykinney on 4/16/15.
 */
class HighchartsWebPlotter extends WebPlotter[HighchartAPI, HighchartAPI] {
  def idFor(plot: HighchartAPI) = plot
  def renderPlot(hc: HighchartAPI) = {
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
