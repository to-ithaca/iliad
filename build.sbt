lazy val coverageSettings = Seq(
	coverageMinimum := 60
)
lazy val buildSettings = Seq(
  organization := "com.ithaca",
  scalaVersion := "2.11.8",
  name         := "iliad",
  version      := "0.0.1-SNAPSHOT"
)

lazy val commonScalacOptions = Seq(
  "-encoding", "UTF-8",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros"
)

lazy val compilerOptions = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-target:jvm-1.7"
  ),
  javacOptions ++= Seq(
    "-source", "1.7", "-target", "1.7"
  )
)

lazy val catsVersion = "0.5.0"
lazy val akkaVersion = "2.3.15"

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= Seq(
    "org.spire-math" %% "imp" % "0.2.0" % "provided",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "co.fs2" %% "fs2-core" % "0.9.0-M1",
    "com.chuusai" %% "shapeless" % "2.2.5",
    "org.spire-math" %% "spire" % "0.11.0",
    "org.typelevel" %% "cats" % catsVersion,
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "org.slf4j" % "slf4j-api" % "1.7.13"
  )
)

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.0-M7" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
    "org.typelevel" %% "discipline" % "0.4" % "test"
  )
)

lazy val paradiseSettings = Seq(
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
)

lazy val macros = (project in file("macros")).settings (
  buildSettings,
  moduleName := "iliad-macros",
  commonSettings,
  testSettings,
  paradiseSettings,
  coverageSettings
)

lazy val core = (project in file("core")).settings(
  buildSettings,
  moduleName := "iliad-core",
  commonSettings,
  paradiseSettings,
  coverageSettings,	
  testSettings
).dependsOn(macros, kernel)

lazy val kernel = (project in file("kernel")).settings(
  buildSettings,
  moduleName := "iliad-kernel",
  commonSettings,
  paradiseSettings
)

lazy val androidKernel = (project in file("kernel-android")).settings(
  buildSettings,
  moduleName := "iliad-kernel-android",
  commonSettings,
  paradiseSettings
).dependsOn(kernel)

lazy val win32Kernel = (project in file("kernel-win32")).settings(
  buildSettings,
  moduleName := "iliad-kernel-win32",
  commonSettings,
  paradiseSettings
).dependsOn(kernel)

lazy val iosKernel = (project in file("kernel-ios")).settings(
  buildSettings,
  moduleName := "iliad-kernel-ios",
  commonSettings,
  paradiseSettings
).dependsOn(kernel)

lazy val root = (project in file(".")).settings(
  buildSettings,
  moduleName := "iliad"
).aggregate(macros, core, kernel)

