package wisp.highcharts

import wisp.Plotter

/**
 * Created by rodneykinney on 4/18/15.
 */
class HistogramAPI(config: RootConfig,
                   plotter: Plotter[RootPlot, Int]) extends RootAPI(config, plotter) {
  private var numBins = 50
}

