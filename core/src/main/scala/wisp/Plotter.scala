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
  def plots: Seq[T]
}

abstract class WebPlotter[T, TRef] extends Plotter[T, TRef] {
  def idFor(plot: T): TRef
  def renderPlot(plot: T): String

  var plots = Vector[T]()
  private val indexFor = collection.mutable.Map.empty[TRef, Int]

  def addPlot(plot: T) = {
    plots = plots :+ plot
    refresh()
    val id = idFor(plot)
    indexFor(id) = plots.size - 1
    id
  }

  def updatePlot(id: TRef, newPlot: T) = {
    val idx = indexFor.remove(id).get
    plots = plots.updated(idx, newPlot)
    refresh()
    val newId = idFor(newPlot)
    indexFor(newId) = idx
    newId
  }

  private var serverRootFileName = s"index-${System.currentTimeMillis()}.html"
  private var port = Port.any
  private var serverMode = false
  private var firstOpenWindow = false

  private var serverRootFile = new File(serverRootFileName)

  var http: Option[Server] = None
  var plotServer: Option[PlotServer] = None

  startWispServer()

  /**
   *
   * @return
   */
  def getWispServerInfo(): (File, Int, Boolean) = {
    (serverRootFile, port, serverMode)
  }

  def setWispServerFile(filename: String): Unit = {
    stopWispServer
    this.serverRootFileName = filename
    this.serverRootFile = new File(serverRootFileName)
    startWispServer()
  }

  def setWispServerFile(file: File): Unit = {
    setWispServerFile(file.getAbsolutePath())
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
  def startWispServer(message: String = s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}/${serverRootFileName}") {
    if (!serverMode) {
      serverMode = true
      val ps = new PlotServer
      val args = UnfilteredWebApp.Arguments(altRoot = Some(serverRootFile.getAbsolutePath), port = port)
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
      serverRootFile.delete()
      // satisfy the promise, to avoid exception on close
      // TODO handle failure in the PlotServer
      plotServer.map(_.p.success(()))
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

    val fileContents = buildHtmlFile()

    val temp = File.createTempFile("highcharts", ".html")
    val pw = new PrintWriter(temp)
    pw.print(fileContents)
    pw.flush()
    pw.close()

    plotServer.foreach { ps =>
      ps.p.success(())
      ps.p = Promise[Unit]()
    }

    val (serverRootFile, port, serverMode) = getWispServerInfo()

    lazy val link =
      if (serverMode) {
        FileUtils.deleteQuietly(serverRootFile)
        FileUtils.moveFile(temp, serverRootFile)
        s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}"
      } else s"file://$temp"

    openFirstWindow(link)

    println(s"Output written to $link (CMD + Click link in Mac OSX).")
  }

  def reloadJs =
    """
      |<script type="text/javascript">$.ajax({url: '/check', dataType: 'jsonp', complete: function(){location.reload()}})</script>
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

