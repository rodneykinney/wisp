package wisp

import java.io.{File, PrintWriter}

import wisp.highcharts.HighchartsJson._
import wisp.highcharts.RootConfig
import org.apache.commons.io.FileUtils
import spray.json._
import unfiltered.jetty.Server
import unfiltered.util.Port
import wisp.server.{ChartServer, UnfilteredWebApp}

import scala.concurrent.Promise
import scala.util.{Failure, Random, Try}

/**
 * Created by rodneykinney on 4/14/15.
 */
trait ChartDisplay[T, TRef] {
  def addChart(chart: T): TRef

  def updateChart(id: TRef, newChart: T): TRef

  def removeChart(id: TRef): TRef

  def charts: Seq[T]
}

abstract class HtmlChartDisplay[P, S] extends ChartDisplay[P, Int] {
  private var chartVector = Vector.empty[Option[(P, S)]]
  private var commandHistory = Vector.empty[Command]
  private var lastCommand = -1

  def getChartConfig(chart: P): S

  def setChartConfig(chart: P, state: S)

  def renderChart(state: S): String

  def charts = chartVector.flatten.map(_._1)

  def addChart(chart: P) = {
    val idx = chartVector.size
    executeCommand(Add(idx, chart, getChartConfig(chart)))
    refresh()
    idx
  }

  def updateChart(idx: Int, newChart: P) = {
    if (idx >= 0 && idx < chartVector.size) {
      val (chart, state) = chartVector(idx).get
      executeCommand(Update(idx, newChart, state, getChartConfig(newChart)))
      refresh()
    }
    idx
  }

  def executeCommand(cmd: Command) = {
    commandHistory = commandHistory.take(lastCommand + 1) :+ cmd
    lastCommand += 1
    cmd.execute
  }

  def removeChart(idx: Int) = {
    if (idx >= 0 && idx < chartVector.size) {
      chartVector(idx) match {
        case Some((chart, state)) =>
          executeCommand(Remove(idx, chart, state))
          refresh()
        case None => ()
      }
    }
    idx
  }

  def undo() = {
    if (lastCommand >= 0) {
      commandHistory(lastCommand).undo.execute
      lastCommand -= 1
      refresh()
    }
    else {
      println("Nothing to undo")
    }
  }

  def redo() = {
    if (lastCommand < commandHistory.size - 1) {
      commandHistory(lastCommand + 1).execute
      lastCommand += 1
      refresh()
    }
    else {
      println("Nothing to redo")
    }
  }

  def clear() =
    for ((Some(p), i) <- chartVector.zipWithIndex) {
      removeChart(i)
    }

  private var port = Port.any
  private var serverMode = false
  private var firstOpenWindow = false

  var http: Option[Server] = None
  var chartServer: Option[ChartServer] = None

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
   * If this is the first chart command being called, try to open the browser
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
   * Launches the server which hosts the charts. InetAddress.getLocalHost requires a properly configured /etc/hosts
   * on linux machines.
   * Assigns a random port
   * @param message
   */
  def startWispServer(message: String = s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}/") {
    if (!serverMode) {
      serverMode = true
      val ps = new ChartServer
      val args = UnfilteredWebApp.Arguments(altRoot = None, port = port)
      val server = ps.get(args)
      server.start
      println("Server started: " + message)
      http = Some(server)
      chartServer = Some(ps)
    }
  }

  /**
   * Deletes the resulting index-*.html and stops the server
   * Currently the index-*.html file persists in the $cwd if stopServer is not called.
   */
  def stopWispServer {
    if (serverMode) {
      chartServer.map(_.refresh("Shutting down...", ""))
      http.map(_.stop)
      http.map(_.destroy)
      serverMode = false
      chartServer = None
    }
  }

  /**
   * Iterates through the charts and builds the necessary javascript and html around them.
   * returns the files contents as a string
   */
  def buildHtmlFile(): String = {
    val sb = new StringBuilder()
    sb.append(jsHeader)
    sb.append(reloadJs)
    sb.append("</head>")
    sb.append("<body>")
    chartVector.flatten.map(_._2).map(renderChart).foreach(sb.append)
    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  def refresh(): Unit = {

    val contentWithPlaceholder = buildHtmlFile()
    val contentHash = contentWithPlaceholder.hashCode.toHexString
    val actualContent = contentWithPlaceholder.replaceAllLiterally("HASH_PLACEHOLDER", contentHash)

    chartServer.map(_.refresh(actualContent, contentHash))

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


  trait Command {
    def execute: Unit

    def undo: Command
  }

  case class Add(idx: Int, chart: P, state: S) extends Command {
    def execute = {
      if (idx == chartVector.size)
        chartVector = chartVector :+ Some((chart, state))
      else if (idx < chartVector.size)
        chartVector = chartVector.updated(idx, Some((chart, state)))
    }

    def undo = Remove(idx, chart, state)
  }

  case class Remove(idx: Int, chart: P, state: S) extends Command {
    def execute = {
      chartVector = chartVector.updated(idx, None)
    }

    def undo = Add(idx, chart, state)
  }

  case class Update(idx: Int, chart: P, oldState: S, newState: S) extends Command {
    def execute = {
      setChartConfig(chart, newState)
      chartVector = chartVector.updated(idx, Some((chart, newState)))
    }

    def undo = Update(idx, chart, newState, oldState)
  }

}

