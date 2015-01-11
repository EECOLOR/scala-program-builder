name := "scala-program-builder"

organization := "org.qirx"

version := "0.2-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.qirx" %% "little-spec" % "0.4" % "test"
)

testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework")

//scalacOptions += "-Xlog-implicits"

//fork := true

//javaOptions in (Test) += "-Xdebug"

//javaOptions in (Test) += "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005"
