package org.qirx.programbuilder

import scala.language.higherKinds

trait ToParameterized[T] {
  type Out[_]
}

object ToParameterized {
  implicit def withEmpty[F[_]]: ToParameterized[F +: Nil] {
    type Out[A] = F[A]
  } = null

  implicit def typeSet[F[_], G[_], X](
    implicit to: ToParameterized[G +: X]): ToParameterized[F +: G +: X] {
    type Out[A] = Coproduct[F, to.Out]#Instance[A]
  } = null
}