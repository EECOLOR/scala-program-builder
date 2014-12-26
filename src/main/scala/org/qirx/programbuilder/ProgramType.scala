package org.qirx.programbuilder

import scala.language.higherKinds
import org.qirx.programbuilder.branching.Branch

final class ProgramType[F[_]] {
  type Out[x] = F[x]
  
  def withBranch[T](implicit add: Coproduct.TypeBasedAdd[F, Branch[T]#Instance]) =
    new ProgramType[add.Out]
}

object ProgramType {
  def apply[T](implicit to: ToParameterized[T]) = new ProgramType[to.Out]

  trait ToParameterized[T] {
    type Out[_]
  }

  object ToParameterized {
    implicit def singleType[F[_]]: ToParameterized[F :: Nil] {
      type Out[x] = F[x]
    } = null

    implicit def multipleTypes[F[_], G[_], X](
      implicit to: ToParameterized[G :: X]): ToParameterized[F :: G :: X] {
      type Out[x] = Coproduct[F, to.Out]#Instance[x]
    } = null
  }
}