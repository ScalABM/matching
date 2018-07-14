// define some common build settings used by core auctions API as well as the various testing configurations
lazy val commonSettings = Seq(
  scalaVersion := "2.12.4" ,
  name := "esl-matching",
  version := "0.1.0-SNAPSHOT",
  organization := "org.economicsl",
  organizationName := "EconomicSL",
  organizationHomepage := Some(url("https://economicsl.github.io/")),
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.0.5",
    "org.typelevel" %% "cats-core" % "1.1.0",
    "com.typesafe.akka" %% "akka-actor" % "2.5.6",
    "org.economicsl" %% "esl-core" % "0.1.0-SNAPSHOT"
  ),
  resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
  ),
  scalacOptions ++= Seq(
    "-deprecation",  // issue warning if we use any deprecated API features
    "-feature",  // tells the compiler to provide information about misused language features
    "-language:implicitConversions",  // eliminates the need to import implicit conversions for each usage
    "-language:reflectiveCalls",  // needed in order to enable structural (or duck) typing
    "-Xlint",
    "-Ywarn-unused-import",
    "-Ywarn-dead-code",
    "-Ypartial-unification"
  )
)


// Define additional testing configurations
lazy val Functional = config("functional") extend Test


lazy val Performance = config("performance") extend Test


// finally define the full project build settings
lazy val core = (project in file(".")).
  settings(commonSettings: _*).
  configs(Functional).
  settings(inConfig(Functional)(Defaults.testSettings): _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "functional, test",
      "org.scalatest" %% "scalatest" % "3.0.1" % "functional, test",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.6" % "functional, test"
    ),
    parallelExecution in Functional := true
  ).
  configs(Performance).
  settings(inConfig(Performance)(Defaults.testSettings): _*).
  settings(
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2" % "performance",
    parallelExecution in Performance := false
  )
