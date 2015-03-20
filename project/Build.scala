import sbt._
import Keys._
import xerial.sbt.Sonatype.SonatypeKeys._
import sbtrelease.ReleasePlugin

object WispBuild extends Build {

	lazy val wisp = Project("wisp", file("core"), settings = wispSettings)

	def sharedSettings = Defaults.defaultSettings ++
		ReleasePlugin.releaseSettings ++
//		BranchRelease.branchSettings ++
		xerial.sbt.Sonatype.sonatypeSettings ++
		Seq(
			scalaVersion := "2.11.4",
			crossScalaVersions := Seq("2.10.4", "2.11.4"),
			organization := "com.quantifind",
			scalacOptions := Seq("-deprecation", "-unchecked", "-optimize"),
			retrieveManaged := true,
			transitiveClassifiers in Scope.GlobalScope := Seq("sources"),
			resolvers ++= Seq(
				"sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
				"sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases"
			),

			libraryDependencies ++= Seq(
				"org.scalatest" %% "scalatest" % "2.2.1" % "test"
			),
			javacOptions ++= Seq("-target", "1.6", "-source", "1.6")
		)

	def wispSettings = sharedSettings ++ Seq(
		name := "Wisp",
		libraryDependencies ++= Seq(
			"org.json4s" %% "json4s-jackson" % "3.2.10",
			"net.databinder" %% "unfiltered-filter" % "0.8.3",
			"net.databinder" %% "unfiltered-jetty" % "0.8.3",
			"com.quantifind" %% "sumac" % "0.3.0",
			"org.apache.commons" % "commons-math3" % "3.1",
			"commons-io" % "commons-io" % "2.4"
		)
	)

	publishMavenStyle := true

	publishTo <<= version { (v: String) =>
		val nexus = "https://oss.sonatype.org/"
		if (v.trim.endsWith("SNAPSHOT"))
			Some("snapshots" at nexus + "content/repositories/snapshots")
		else
			Some("releases"  at nexus + "service/local/staging/deploy/maven2")
	}

	publishArtifact in Test := false
	profileName := "com.quantifind"
	pomIncludeRepository := { x => false }
	pomExtra := (
		<url>https://github.com/quantifind/wisp</url>
			<licenses>
				<license>
					<name>Apache 2</name>
					<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
					<distribution>repo</distribution>
					<comments>A business-friendly OSS license</comments>
				</license>
			</licenses>
			<scm>
				<url>git@github.com:quantifind/wisp.git</url>
				<connection>scm:git:git@github.com:quantifind/wisp.git</connection>
			</scm>
			<developers>
				<developer>
					<id>Austin</id>
					<name>Austin Gibbons</name>
					<url>http://github.com/austinbgibbons</url>
				</developer>
			</developers>)
}
