lazy val root = (project in file(".")).
  settings(
    name := "Functional Programming in Scala",
    version := "1.0",
    scalaVersion := "2.12.1"
  )

libraryDependencies += "io.monix" %% "minitest" % "2.0.0" % "test"

testFrameworks += new TestFramework("minitest.runner.Framework")

