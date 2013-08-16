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
  "org.scalaz"      % "scalaz-core_2.10"       % "7.0.2",
  "org.scalaz"      % "scalaz-effect_2.10"     % "7.0.2",
  "com.chuusai"    %% "shapeless"              % "1.2.4",
  "org.spire-math" %% "spire"                  % "0.5.0"
)

// Test
libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck"           % "1.10.0"       % "test",
  "org.specs2"     %% "specs2"               % "1.13"         % "test"
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
     import time.calendar._"""


