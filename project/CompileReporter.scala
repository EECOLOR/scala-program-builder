package sbt

object qirx {
  val setting = Keys.compilerReporter := Some(CompileReporter)
}

object CompileReporter extends xsbti.Reporter {
  def comment(pos: xsbti.Position, msg: String): Unit = ()
  def hasErrors(): Boolean = false
  def hasWarnings(): Boolean = false
  def log(pos: xsbti.Position, msg: String, sev: xsbti.Severity): Unit = ()
  def printSummary(): Unit = ()
  def problems(): Array[xsbti.Problem] = Array.empty
  def reset(): Unit = ()
}