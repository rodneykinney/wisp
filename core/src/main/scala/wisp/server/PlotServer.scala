package wisp.server

import unfiltered.request._
import unfiltered.response._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

/**
 * User: austin
 * Date: 12/1/14
 *
 * An unfiltered web-app for displaying graphs
 */
class PlotServer extends UnfilteredWebApp[UnfilteredWebApp.Arguments] {
  // this is fulfilled by the plot command, to allow a browser to wait for plot to reload
  private var p = Promise[Unit]()
  private var content = "Initializing..."
  private var contentHash = ""
  def refresh(newContent: String, newContentHash: String) = {
    content = newContent
    contentHash = newContentHash
    p.success()
    p = Promise[Unit]()
  }

  private class WebApp extends unfiltered.filter.Plan {
    def intent = {
      // handle jsonp
      case req@GET(Path(Seg("check" :: Nil)) & Params(params)) =>
        val clientContentHash = params.values.headOption.map(_.headOption).flatten
        implicit val responder = req
        val response = s""""$contentHash""""
        // block for plot command to fulfill promise, and release this result to trigger browser reload
        if (clientContentHash.forall(_ == contentHash)) {
          Await.result(p.future, Duration.Inf)
        }
        JsonContent ~> ResponseString(response)
      case req@GET(Path(Seg(Nil)) & Params(params)) =>
        implicit val responder = req
        HtmlContent ~> ResponseString(content)
      case _ => Pass
    }
  }

  def setup(parsed: UnfilteredWebApp.Arguments): unfiltered.filter.Plan = {
    new WebApp
  }

  def htmlRoot: String = "/"
}