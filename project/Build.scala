import sbt._
import Keys._
import com.typesafe.sbt.SbtPgp.PgpKeys.publishSigned

import xerial.sbt.Sonatype.SonatypeKeys._

import sbtrelease.ReleaseStateTransformations._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleasePlugin
import sbtrelease.ReleaseStep
import sbtrelease.Utilities._
import sbtrelease.Vcs
import sbtrelease.releaseTask

import annotation.tailrec


object WispBuild extends Build {

	lazy val wisp = Project("wisp", file("core"), settings = wispSettings)

	def sharedSettings = Defaults.defaultSettings ++
		ReleasePlugin.releaseSettings ++
		BranchRelease.branchSettings ++
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

			releaseProcess := Seq[ReleaseStep](
				checkSnapshotDependencies,
				inquireVersions,
				runTest,
				setReleaseVersion,
				BranchRelease.makeBranch,           //make a new rel/$version branch
				commitReleaseVersion,               // all changes happen here
				tagRelease,                         //we tag where the next release starts from
				BranchRelease.publishSignedArtifacts,
				releaseTask(sonatypeReleaseAll),
				BranchRelease.pushBranch,
				BranchRelease.moveToPreviousBranch,
				setNextVersion,                     // bump to the next snapshot version
				commitNextVersion,
				pushChanges
			),

			publishMavenStyle := true,

			publishTo <<= version { (v: String) =>
				val nexus = "https://oss.sonatype.org/"
				if (v.trim.endsWith("SNAPSHOT"))
					Some("snapshots" at nexus + "content/repositories/snapshots")
				else
					Some("releases"  at nexus + "service/local/staging/deploy/maven2")
			},

			publishArtifact in Test := false,
			profileName := "com.quantifind",
			pomIncludeRepository := { x => false },
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
					</developers>),
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
}


object BranchRelease {

	import BranchReleaseKeys._

	//> copy from sbtrelease some usefule private methods
	private def vcs(st: State): Vcs = {
		st.extract
			.get(versionControlSystem)
			.getOrElse(sys.error("Aborting release. Working directory is not a"+
			"repository of a recognized VCS."))
	}

	private lazy val initialVcsChecks = { st: State =>
		val status = (vcs(st).status !!).trim
		if (status.nonEmpty) {
			sys.error("Aborting release. Working directory is dirty.")
		}
		st
	}

	private lazy val checkUpstream = { st: State =>
		if (!vcs(st).hasUpstream) {
			sys.error("No tracking branch is set up. Either configure a remote tracking branch, or remove the pushChanges release part.")
		}

		st.log.info("Checking remote [%s] ..." format vcs(st).trackingRemote)
		if (vcs(st).checkRemote(vcs(st).trackingRemote) ! st.log != 0) {
			SimpleReader.readLine("Error while checking remote. Still continue (y/n)? [n] ") match {
				case Yes() => // do nothing
				case _ => sys.error("Aborting the release!")
			}
		}

		if (vcs(st).isBehindRemote) {
			SimpleReader.readLine("The upstream branch has unmerged commits. A subsequent push will fail! Continue (y/n)? [n] ") match {
				case Yes() => // do nothing
				case _ => sys.error("Merge the upstream commits and run `release` again.")
			}
		}
		st
	}
	//<copy from sbtrelease

	//////////////////////////////////////////////////////////////////////
	// a release step that branches from the current branch
	//////////////////////////////////////////////////////////////////////
	lazy val makeBranch = ReleaseStep(makeBranchAction, initialVcsChecks)

	private lazy val makeBranchAction = { st: State =>
		val vc = vcs(st)

		//store the current branch so we can come back here
		val nst = st.put(previousBranchKey, vc.currentBranch)

		//make sure that the new branch doesn't exist yet
		@tailrec
		def testBranch(branch: String): String = {
			def localBranchExists = {
				nst.log.info(s"checking if local $branch already exists")
				(vc.cmd("branch") !!).linesIterator.exists {
					_.endsWith(s" $branch")
				}
			}
			def remoteBranchExists = {
				nst.log.info(s"checking if remote $branch already exists")
				(vc.cmd("ls-remote", "--heads") !!).linesIterator.exists {
					_.endsWith(s"refs/heads/$branch")
				}
			}

			//if the branch is already defined, get a new name
			if(localBranchExists || remoteBranchExists) {
				SimpleReader
					.readLine(s"Branch [$branch] already exists, [a]bort or specify a new name: ") match {
					case Some("" | "a" | "A") =>
						sys.error(s"Branch [$branch] already exists. Aborting release!")
					case Some(newBranch) =>
						//test the entered name
						testBranch(newBranch)
					case None =>
						sys.error(s"Branch [$branch] already exists. Aborting release!")
				}
			} else branch
		}

		//get the branch name from config
		val (branchState, branch) = nst.extract.runTask(branchName, nst)
		val branchToUse = testBranch(branch)
		st.log.info("git branching sends its console output to standard error,"+
			"which will cause the next few lines to be marked as [error].")
		vc.cmd("checkout", "-b", branchToUse) !! branchState.log

		//store the new branch to push it later
		branchState.put(branchKey, branchToUse)
	}

	//////////////////////////////////////////////////////////////////////
	//  push the release branch to origin TODO config the remote
	//////////////////////////////////////////////////////////////////////
	lazy val pushBranch = ReleaseStep(pushBranchAction, checkUpstream)
	private lazy val pushBranchAction = { st: State =>
		val vc = vcs(st)

		val b = st.get(branchKey)
			.getOrElse(sys.error("no branch set, you have to run this step after makeBranch"))
		vc.cmd("push", "-u", "origin", b) !! st.log

		st
	}

	lazy val moveToPreviousBranch: ReleaseStep = { st: State =>
		val vc = vcs(st)

		val ba = st.get(previousBranchKey)
			.getOrElse(sys.error("no branch set, you have to run this step after makeBranch"))
		vc.cmd("checkout", ba) !! st.log

		st
	}

	lazy val branchSettings = Seq[Setting[_]](
		branchName <<= (version in ThisBuild) map ( v => s"rel/$v" )
	)

	//////////////////////////////////////////////////////////////////////
	// signed publish
	//////////////////////////////////////////////////////////////////////

	lazy val publishSignedArtifacts = ReleaseStep(
		action = publishSignedArtifactsAction,
		check = st => {
			// getPublishTo fails if no publish repository is set up.
			val ex = st.extract
			val ref = ex.get(thisProjectRef)
			Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
			st
		},
		enableCrossBuild = true
	)
	private lazy val publishSignedArtifactsAction = { st: State =>
		val extracted = st.extract
		val ref = extracted.get(thisProjectRef)
		extracted.runAggregated(publishSigned in Global in ref, st)
	}

}

object BranchReleaseKeys {
	//the name of the rel branch
	lazy val branchName = TaskKey[String]("release-branch-name")
	//used internaly to keep state
	lazy val branchKey = AttributeKey[String]("release-branch")
	lazy val previousBranchKey = AttributeKey[String]("current-branch")
}
