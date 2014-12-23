package org.qirx

import scala.language.higherKinds

package object programbuilder {

  type Id[A] = A

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]

  trait +:[F[_], T]
  trait Nil

  // Simple alias to allow the fancy O - T notation
  type -[O[_], T[_]] = Remove[O, T]
  
  trait ProgramType[F[_]]
  object ProgramType {
    def apply[T](implicit to: ToParameterized[T]): ProgramType[to.Out] = null
    implicit def any[F[_]]:ProgramType[F] = null
  }
}