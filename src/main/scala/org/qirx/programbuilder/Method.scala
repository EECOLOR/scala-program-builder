package org.qirx.programbuilder

trait Method[T] {
  def result:T
}

// __ is here to make sure the Return[_, _] does not match F[_]
case class Return[T, __](result:T) extends Method[T]

object Method {
  implicit object MethodRunner extends (Method ~> Id) {
    def transform[x] = _.result
  }
}