name := "cal"

version := "0.1"

scalaVersion := "2.10.2"

// Set things up so we can run async
fork := true

cancelable := true

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

// Main
libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"       % "7.0.2",
  "org.scalaz"     %% "scalaz-effect"     % "7.0.2",
  "com.chuusai"    %% "shapeless"              % "1.2.4",
  "org.spire-math" %% "spire"                  % "0.5.0"
)

// Test
libraryDependencies ++= Seq(     
  "org.scalacheck" %% "scalacheck"                % "1.10.1" % "test",
  "org.specs2"     %% "specs2"                    % "1.12.3"   % "test",
  "org.typelevel"  %% "scalaz-specs2"             % "0.1.3"  % "test",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.0.2"  % "test"
)

// Let's add a linter
//resolvers += "linter" at "http://hairyfotr.github.io/linteRepo/releases"

//addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

// And turn warnings all the way up
scalacOptions ++= Seq(
	"-feature", 
	"-deprecation", 
	"-Ywarn-all", // doesn't actually turn them all on :-\
	"-Yno-adapted-args",
	"-Ywarn-value-discard", 
//	"-Ywarn-numeric-widen",
//	"-Ywarn-dead-code", // confused by ???, sadly
	"-Xlint",
	"-Xfatal-warnings"
)

initialCommands :=
  """import scalaz._
     import Scalaz._
     import time.calendar._
     import time.calendar.syntax.all._"""
//     val d = Date.fromModifiedJulianDate(56519)"""


