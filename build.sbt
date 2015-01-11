name := "scala-program-builder"

organization := "org.qirx"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.qirx" %% "little-spec" % "0.4" % "test"
)

testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework")

scalacOptions += "-Xlog-implicits"
