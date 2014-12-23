package org.qirx.programbuilder

import scala.language.implicitConversions
import scala.language.higherKinds

trait LowerPriorityImplicits {
  implicit def toProgram[F[_], A](fa: F[A]): Program[F]#Instance[A] =
    Program lift fa
}

object Implicits extends LowerPriorityImplicits {
  implicit def toComplexProgram[F[_], A, O[_]](fa: F[A])(
    implicit programType: ProgramType[O],
    inject: F ~> O): Program[O]#Instance[A] =
    Program lift inject(fa)

  implicit object IdMonadic extends Monadic[Id] {
    def create[A](a: A): Id[A] = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  implicit def identityTransform[F[_]] = new (F ~> F) {
    def transform[A] = identity
  }

  implicit def atCoproductHead[Head[_], Tail[_]] =
    new (Head ~> Coproduct[Head, Tail]#Instance) {
      def transform[A] = fa => Coproduct(Left(fa))
    }

  implicit def inCoproductTail[F[_], Head[_], Tail[_]](
    implicit insertInTail: F ~> Tail) =
    new (F ~> Coproduct[Head, Tail]#Instance) {
      def transform[A] = fa => Coproduct(Right(insertInTail(fa)))
    }

  implicit class NaturalTransformationEnhancements[F[_], O[_]](fToO: F ~> O) {
    def +:[G[_]](gToO: G ~> O) =
      new (Coproduct[G, F]#Instance ~> O) {
        def transform[A] = _.value match {
          case Left(g)  => gToO(g)
          case Right(f) => fToO(f)
        }
      }

    def andThen[G[_]](oToG: O ~> G) =
      new (F ~> G) {
        def transform[A] = fa => oToG(fToO(fa))
      }
  }
  
  implicit class ProgramEnhancement[O[_], A](program: Program[O]#Instance[A]) {
    def mergeBranch(implicit merger: BranchMerger[O, A]) = merger merge program
  }
}