package org.qirx.programbuilder

import scala.language.higherKinds
import org.qirx.programbuilder.branching.Branch

class ProgramType[F[_]] {
  type Out[x] = F[x]

  def withBranch[T](implicit prepend: Coproduct.TypeBasedPrepend[F, Branch[T]#Instance]) =
    new ProgramType[prepend.Out]
}

object ProgramType {
  def apply[T <: Coproduct] = new ProgramType[T#Instance]
}

sealed trait ProgramInformation {
  type Out[_]
  type Parts <: Coproduct
  type WithBranch[T] = With[Out, Branch[T]#Instance :+: Parts]
}

case class With[F[_], C <: Coproduct](val injectors: C#Injectors[F]) extends ProgramType[F] with ProgramInformation {
  type Parts = C
  type Injectors = C#Injectors[F]

  import scala.language.implicitConversions
  implicit def asProgram[F[_], A](fa:F[A]):Program[F]#Instance[A] = null

  //  type WithBranch[T] = With[F, Branch[T]#Instance :+: Coproduct]

  implicit def injector[E[_]](implicit ev: Injectors Contains (E ~> F)): E ~> F =
    ev(injectors)
}

object With {

  type BranchedWith[L <: Coproduct, T] =
    Branch[T]#Instance :+: L

  implicit def freeWith[L <: Coproduct, O[_]](
    implicit
    programType: ProgramType[O],
    ev: L AsInjectorsInto O): O With L =
    With(ev.out)

  trait AsInjectorsInto[L <: Coproduct, O[_]] {
    type Out = L#Injectors[O]
    def out: Out
  }

  object AsInjectorsInto {

    implicit def hNil[O[_]]:CNil AsInjectorsInto O =
      new (CNil AsInjectorsInto O) {
        val out: HNil = HNil
      }

    implicit def mapToTransformation[X[_], L <: Coproduct, O[_]](
      implicit tailTransformations: (L AsInjectorsInto O),
      ev: X ~> O):(X :+: L) AsInjectorsInto O =
      new ((X :+: L) AsInjectorsInto O) {
        val out = ::(ev, tailTransformations.out)
      }

  }
}

