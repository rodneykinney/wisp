package wisp

import java.io.{File, PrintWriter}

import wisp.highcharts.HighchartsJson._
import wisp.highcharts.HighchartAPI
import org.apache.commons.io.FileUtils
import spray.json._
import unfiltered.jetty.Server
import unfiltered.util.Port
import wisp.server.{UnfilteredWebApp, PlotServer}

import scala.concurrent.Promise
import scala.util.{Failure, Random, Try}

/**
 * Created by rodneykinney on 4/14/15.
 */
trait Plotter[T, TRef] {
  def addPlot(plot: T): TRef
  def updatePlot(id: TRef, newPlot: T): TRef
  def removePlot(id: TRef): TRef
  def plots: Seq[T]
}

case class PlotsHolder[T, TRef](plotsWithId: Vector[(TRef, T)] = Vector()) {
  def add(id: TRef, plot: T) =
    PlotsHolder(plotsWithId :+(id, plot))

  def update(id: TRef, newId: TRef, newPlot: T) = {
    val idx = plotsWithId.zipWithIndex.find { case ((elementId, e), idx) => elementId == id }.map(_._2)
    idx match {
      case Some(i) =>
        PlotsHolder(plotsWithId.updated(i, (newId, newPlot)))
      case None =>
        this
    }
  }

  def remove(id: TRef) = {
    PlotsHolder(plotsWithId.filterNot(id == _._1))
  }

  def plots = plotsWithId.map(_._2)
}

abstract class HtmlPlotter[T, TRef] extends Plotter[T, TRef] {
  def idFor(plot: T): TRef
  def renderPlot(plot: T): String

  private val undoRedo = new UndoRedo[PlotsHolder[T, TRef]]
  undoRedo.push(PlotsHolder[T, TRef]())
  private def holder = undoRedo.head.getOrElse(PlotsHolder[T, TRef]())
  def plots = holder.plots

  def addPlot(plot: T) = {
    val id = idFor(plot)
    undoRedo.push(holder.add(id, plot))
    refresh()
    id
  }

  def updatePlot(id: TRef, newPlot: T) = {
    val newId = idFor(newPlot)
    undoRedo.push(holder.update(id, newId, newPlot))
    refresh()
    newId
  }

  def removePlot(id: TRef) = {
    undoRedo.push(holder.remove(id))
    refresh()
    id
  }

  def undo() = {
    undoRedo.undo()
    refresh()
  }

  def redo() = {
    undoRedo.redo()
    refresh()
  }

  private var port = Port.any
  private var serverMode = false
  private var firstOpenWindow = false

  var http: Option[Server] = None
  var plotServer: Option[PlotServer] = None

  startWispServer()

  /**
   *
   * @return
   */
  def getWispServerInfo(): (Int, Boolean) = {
    (port, serverMode)
  }

  def setWispPort(port: Int): Unit = {
    stopWispServer
    this.port = port
    startWispServer()
  }

  def disableOpenWindow(): Unit = {
    this.firstOpenWindow = true
  }

  def openWindow(link: String) = {
    import sys.process._
    Try {
      java.awt.Desktop.getDesktop.browse(new java.net.URI(link))
      link
    }
        .orElse(Try(s"open $link" !!))
        .orElse(Try(s"xdg-open $link" !!))
  }

  /**
   * If this is the first plot command being called, try to open the browser
   * @param link
   */
  def openFirstWindow(link: String) = {
    if (!firstOpenWindow) {
      openWindow(link) match {
        case Failure(msg) =>
          println(s"Error while opening window (cause: $msg)")
          println(s"You can browse the following URL: $link")
        case _ =>
      }
      firstOpenWindow = true
    }
  }

  /**
   * Launches the server which hosts the plots. InetAddress.getLocalHost requires a properly configured /etc/hosts
   * on linux machines.
   * Assigns a random port
   * @param message
   */
  def startWispServer(message: String = s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}/") {
    if (!serverMode) {
      serverMode = true
      val ps = new PlotServer
      val args = UnfilteredWebApp.Arguments(altRoot = None, port = port)
      val server = ps.get(args)
      server.start
      println("Server started: " + message)
      http = Some(server)
      plotServer = Some(ps)
    }
  }

  /**
   * Deletes the resulting index-*.html and stops the server
   * Currently the index-*.html file persists in the $cwd if stopServer is not called.
   */
  def stopWispServer {
    if (serverMode) {
      // satisfy the promise, to avoid exception on close
      // TODO handle failure in the PlotServer
      plotServer.map(_.refresh("Shutting down...", ""))
      http.map(_.stop)
      http.map(_.destroy)
      serverMode = false
      plotServer = None
    }
  }

  /**
   * Iterates through the plots and builds the necessary javascript and html around them.
   * returns the files contents as a string
   */
  def buildHtmlFile(): String = {
    val sb = new StringBuilder()
    sb.append(jsHeader)
    sb.append(reloadJs)
    sb.append("</head>")
    sb.append("<body>")
    plots.map(renderPlot).foreach(sb.append)
    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  def refresh(): Unit = {

    val contentWithPlaceholder = buildHtmlFile()
    val contentHash = contentWithPlaceholder.hashCode.toHexString
    val actualContent = contentWithPlaceholder.replaceAllLiterally("HASH_PLACEHOLDER", contentHash)

    plotServer.map(_.refresh(actualContent, contentHash))

    val (port, serverMode) = getWispServerInfo()

    openFirstWindow(s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}")
  }

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

  val wispJsImports: String =
    """
      |<script type="text/javascript" src="http://code.jquery.com/jquery-1.8.2.min.js"></script>
      |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/highcharts.js"></script>
      |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/modules/exporting.js"></script>
      |<script type="text/javascript" src="http://code.highcharts.com/4.0.4/highcharts-more.js"></script>
    """.stripMargin

  val jsHeader =
    """
      |<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
      |<html>
      |  <head>
      |    <title>
      |      HighchartAPI
      |    </title>
      |    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    """.stripMargin +
        wispJsImports


}

