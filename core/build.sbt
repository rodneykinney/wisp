scalaVersion := "2.11.5"

name := "Wisp"

libraryDependencies ++= Seq(
	"org.json4s" %% "json4s-jackson" % "3.2.10",
	"net.databinder" %% "unfiltered-filter" % "0.8.3",
	"net.databinder" %% "unfiltered-jetty" % "0.8.3",
	"com.quantifind" %% "sumac" % "0.3.0",
	"org.apache.commons" % "commons-math3" % "3.4.1",
	"commons-io" % "commons-io" % "2.4",
	"org.scalatest" %% "scalatest" % "2.1.3" % "test",
	"io.spray" %%  "spray-json" % "1.3.1"
	)
