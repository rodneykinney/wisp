package wisp.server

import unfiltered.request._
import unfiltered.response._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

class ChartServer(port: Int) {
  val httpServer = unfiltered.jetty.Server.http(port).plan(new WebApp)
  httpServer.start()

  private var p = Promise[Unit]()
  private var content = "Initializing..."
  private var contentHash = ""

  def refresh(newContent: String,
              newContentHash: String) = {
    content = newContent
    contentHash = newContentHash
    p.success()
    p = Promise[Unit]()
  }

  private class WebApp extends unfiltered.filter.Plan {
    def intent = {
      case req@GET(Path(Seg("check" :: Nil)) & Params(params)) =>
        val clientContentHash = params.values.headOption.map(_.headOption).flatten
        implicit val responder = req
        val response = s""""$contentHash""""
        // If content on the server side is the same as loaded by the client, block
        // Block will be released on calling refresh()
        if (clientContentHash.forall(_ == contentHash)) {
          try {
            Await.result(p.future, Duration.Inf)
          }
          catch {
            case ex: InterruptedException => () // Shutdown
          }
        }
        JsonContent ~> ResponseString(response)
      case req@GET(Path(Seg(Nil)) & Params(params)) =>
        implicit val responder = req
        HtmlContent ~> ResponseString(content)
      case _ => Pass
    }
  }

  def stop(): Unit = {
    httpServer.stop
    httpServer.destroy
  }
}