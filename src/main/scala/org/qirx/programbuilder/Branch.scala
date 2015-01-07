package org.qirx.programbuilder.branching

/**
 * Represents a branch in a program. It essentially is the `Left` part of an either.
 * 
 * It is encoded with `Instance[R]` so it will match `F[_]` which helps with 
 * implicit resolution.
 */
class Branch[L] {
  case class Instance[R](value: L)
}

object Branch {
  /**
   * Utility method to create a branch instance.
   */
  def apply[L, R](value: L) = new Branch[L].Instance[R](value)
}