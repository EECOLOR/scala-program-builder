package org.qirx.programbuilder

trait Static[T] {
  def result:T
}

// __ is here to make sure the Return[_, _] does not match F[_]
case class Return[T, __](result:T) extends Static[T]

object Static {
  implicit object Runner extends (Static ~> Id) {
    def transform[x] = _.result
  }
}