name := "scala-program-builder"

organization := "org.qirx"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.qirx" %% "little-spec" % "0.4" % "test"
)

testFrameworks += new TestFramework("org.qirx.littlespec.sbt.TestFramework")
