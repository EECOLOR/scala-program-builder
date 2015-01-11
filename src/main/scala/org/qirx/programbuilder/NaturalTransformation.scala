package org.qirx.programbuilder

import scala.language.higherKinds
import scala.concurrent.Future

trait NaturalTransformation[-F[_], +G[_]] { fToG =>
  def transform[x]: F[x] => G[x]

  def apply[x](fx: F[x]): G[x] = transform[x](fx)
}

object NaturalTransformation extends DefaultNaturalTransformations

trait DefaultNaturalTransformations extends CoproductInjectors {

  implicit object IdToFuture extends (Id ~> Future) {
    def transform[x] = Future successful _
  }

  implicit def idToEither[L]: Id ~> ({ type T[x] = Either[L, x] })#T =
    new (Id ~> ({ type T[x] = Either[L, x] })#T) {
      def transform[x] = Right apply _
    }

  implicit object OptionToSeq extends (Option ~> Seq) {
    def transform[x] = _.toSeq
  }
}

trait CoproductInjectors extends LowerPriorityCoproductInjectors {
  implicit def inCoproductTail[F[_], Head[_], Tail[_]](
    implicit injectInTail: F ~> Tail): F ~> Coproduct[Head, Tail]#Instance =
    new (F ~> Coproduct[Head, Tail]#Instance) {
      def transform[A] = fa => Coproduct(Right(injectInTail(fa)))
    }
}

trait LowerPriorityCoproductInjectors extends CoproductTypeAlignment {
  implicit def atCoproductHead[Head[_], Tail[_]]: Head ~> Coproduct[Head, Tail]#Instance =
    new (Head ~> Coproduct[Head, Tail]#Instance) {
      def transform[A] = fa => Coproduct(Left(fa))
    }
}

trait CoproductTypeAlignment extends LowerPriorityDefaultNaturalTransformations {
  implicit def coproduct[Head[_], Tail[_], T[_]](
    implicit injectHead: Head ~> T,
    injectTail: Tail ~> T): (Coproduct[Head, Tail]#Instance ~> T) =
    new (Coproduct[Head, Tail]#Instance ~> T) {
      def transform[x] = _.value match {
        case Left(head)  => injectHead(head)
        case Right(tail) => injectTail(tail)
      }
    }
}

trait LowerPriorityDefaultNaturalTransformations {
  implicit def identityTransform[F[_]]: F ~> F = new (F ~> F) {
    def transform[x] = identity
  }
}
