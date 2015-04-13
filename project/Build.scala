import sbt._


import annotation.tailrec


object WispBuild extends Build {

	lazy val wisp = Project("wisp", file("core"), settings = Defaults.defaultSettings)
}


