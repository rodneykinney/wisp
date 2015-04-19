package wisp.highcharts

import wisp.HtmlChartDisplay

import scala.util.Random
import spray.json._
import HighchartsJson._

/**
 * Created by rodneykinney on 4/16/15.
 */
class HighchartsHtmlDisplay extends HtmlChartDisplay[RootChart, RootConfig] {

  override def getChartConfig(plot: RootChart): RootConfig = plot.config

  override def setChartConfig(plot: RootChart, state: RootConfig): Unit = plot.config = state

  def renderChartsToHtml(): String = {

    val sb = new StringBuilder()

    val jsHeader =
      """
        |<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
        |<html>
        |  <head>
        |    <title>
        |      HighchartAPI
        |    </title>
        |    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
      """.stripMargin

    sb.append(jsHeader)

    val imports: String =
      """
        |<script type="text/javascript" src="http://code.jquery.com/jquery-1.8.2.min.js"></script>
        |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/highcharts.js"></script>
        |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/modules/exporting.js"></script>
        |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/highcharts-more.js"></script>
      """.stripMargin

    sb.append(imports)

    def reloadJs =
      """
        |<script type="text/javascript">
        |var contentHash = 'HASH_PLACEHOLDER';
        |$.ajax({
        |  url: '/check',
        |  data: {'clientContentHash' : [contentHash]},
        |  success: function(result) {
        |    location.reload();
        |  }})
        |</script>
      """.stripMargin


    sb.append(reloadJs)

    sb.append("</head>")
    sb.append("<body>")
    chartConfigs.map(renderChart).foreach(sb.append)
    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }


  def renderChart(hc: RootConfig) = {
    val json = hc.toJson.toString
    val containerId = json.hashCode.toHexString
    s"""
       |<div id="container$containerId"></div>
                                        |   <script type="text/javascript">
                                        |       $$(function() {
                                                    |           $$('#container$containerId').highcharts(
                                                                                            |               $json
        |           );
        |       });
        |   </script>
        |
        """.stripMargin
  }

}
