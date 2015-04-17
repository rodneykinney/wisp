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
    val hash = hc.hashCode()
    val containerId = Random.nextInt(1e10.toInt) + (if (hash < 0) -1 else 1) * hash // salt the hash to allow duplicates
    val json = hc.toJson.toString
    s"""
       | <div id="container%s"></div>
    """.stripMargin.format(containerId) + "\n" +
        """
          |    <script type="text/javascript">
          |        $(function() {
          |            $('#container%s').highcharts(
        """.stripMargin.format(containerId) +
        """
          |                %s
          |            );
          |        });
          |    </script>
          |
        """.stripMargin.format(json)
  }

}
