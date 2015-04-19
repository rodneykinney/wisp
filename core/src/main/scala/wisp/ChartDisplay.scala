package wisp

import unfiltered.util.Port
import wisp.server.ChartServer

import scala.util.{Failure, Try}

/**
 * Created by rodneykinney on 4/14/15.
 */
trait ChartDisplay[T, TRef] {
  def addChart(chart: T): TRef

  def updateChart(id: TRef, newChart: T): TRef

  def removeChart(id: TRef): TRef

  def charts: Seq[T]
}

trait UndoableChartDisplay[TChart, TConfig] extends ChartDisplay[TChart, Int] {
  def getChartConfig(chart: TChart): TConfig

  def setChartConfig(chart: TChart, config: TConfig)

  def refresh(): Unit

  private var chartVector = Vector.empty[Option[(TChart, TConfig)]]
  private var commandHistory = Vector.empty[Command]
  private var lastCommand = -1

  def charts = chartVector.flatten.map(_._1)

  def chartConfigs = chartVector.flatten.map(_._2)

  def addChart(chart: TChart) = {
    val idx = chartVector.size
    executeCommand(Add(idx, chart, getChartConfig(chart)))
    refresh()
    idx
  }

  def updateChart(idx: Int, newChart: TChart) = {
    if (idx >= 0 && idx < chartVector.size) {
      val (chart, config) = chartVector(idx).get
      executeCommand(Update(idx, newChart, config, getChartConfig(newChart)))
      refresh()
    }
    idx
  }

  def removeChart(idx: Int) = {
    if (idx >= 0 && idx < chartVector.size) {
      chartVector(idx) match {
        case Some((chart, config)) =>
          executeCommand(Remove(idx, chart, config))
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

  def executeCommand(cmd: Command) = {
    commandHistory = commandHistory.take(lastCommand + 1) :+ cmd
    lastCommand += 1
    cmd.execute
  }

  trait Command {
    def execute: Unit

    def undo: Command
  }

  case class Add(idx: Int, chart: TChart, config: TConfig) extends Command {
    def execute = {
      if (idx == chartVector.size)
        chartVector = chartVector :+ Some((chart, config))
      else if (idx < chartVector.size)
        chartVector = chartVector.updated(idx, Some((chart, config)))
    }

    def undo = Remove(idx, chart, config)
  }

  case class Remove(idx: Int, chart: TChart, config: TConfig) extends Command {
    def execute = {
      chartVector = chartVector.updated(idx, None)
    }

    def undo = Add(idx, chart, config)
  }

  case class Update(idx: Int, chart: TChart, oldConfig: TConfig, newConfig: TConfig) extends Command {
    def execute = {
      setChartConfig(chart, newConfig)
      chartVector = chartVector.updated(idx, Some((chart, newConfig)))
    }

    def undo = Update(idx, chart, newConfig, oldConfig)
  }



}

abstract class HtmlChartDisplay[TChart, TConfig] extends UndoableChartDisplay[TChart, TConfig] {
  def renderChartsToHtml(): String

  private var port = Port.any
  private var browserLaunched = false

  var chartServer: Option[ChartServer] = None

  def serverRunning = chartServer.isDefined

  startServer()

  def setPort(port: Int): Unit = {
    this.port = port
    if (serverRunning) {
      stopServer
      startServer()
    }
  }

  def openWindow(link: String) = {
    import scala.sys.process._
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
  def launchBrowser(link: String) = {
    if (!browserLaunched) {
      openWindow(link) match {
        case Failure(msg) =>
          println(s"Error while opening window (cause: $msg)")
          println(s"You can browse the following URL: $link")
        case _ =>
      }
      browserLaunched = true
    }
  }

  /**
   * Launches the server which hosts the charts. InetAddress.getLocalHost requires a properly configured /etc/hosts
   * on linux machines.
   * Assigns a random port
   */
  def startServer() {
    if (!chartServer.isDefined) {
      chartServer = Some(new ChartServer(port))
      println(s"Server started at http://${
        java.net.InetAddress.getLocalHost.getCanonicalHostName
      }:${port}")
    }
  }

  def stopServer {
    chartServer.map(_.stop)
    chartServer = None
  }

  def refresh(): Unit = {

    val contentWithPlaceholder = renderChartsToHtml()
    val contentHash = contentWithPlaceholder.hashCode.toHexString
    val actualContent = contentWithPlaceholder.replaceAllLiterally("HASH_PLACEHOLDER", contentHash)

    chartServer.map(_.refresh(actualContent, contentHash))

    launchBrowser(s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}")
  }


}

