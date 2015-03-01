package org.qirx.programbuilder

import scala.language.higherKinds
import scala.concurrent.Future

trait NaturalTransformation[-F[_], +G[_]] { fToG =>
  def transform[x]: F[x] => G[x]

  def apply[x](fx: F[x]): G[x] = transform[x](fx)
}

object NaturalTransformation extends DefaultNaturalTransformations

// The order of the implicits increases compile performance
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
  implicit def atCoproductHead[Head[_], Tail <: Coproduct]: Head ~> (Head :+: Tail)#Instance =
    new (Head ~> (Head :+: Tail)#Instance) {
      def transform[A] = fa => Coproduct.Head(fa)
    }
}

trait LowerPriorityCoproductInjectors extends CoproductTypeAlignment {
  implicit def inCoproductTail[F[_], Head[_], Tail <: Coproduct](
    implicit injectInTail: F ~> Tail#Instance): F ~> (Head :+: Tail)#Instance =
    new (F ~> (Head :+: Tail)#Instance) {
      def transform[A] = fa => Coproduct.Tail(injectInTail(fa))
    }
}

trait CoproductTypeAlignment extends LowerPriorityDefaultNaturalTransformations {
  implicit def coproduct[Head[_], Tail <: Coproduct, T[_]](
    implicit injectHead: Head ~> T,
    injectTail: Tail#Instance ~> T): (Head :+: Tail)#Instance ~> T =
    new ((Head :+: Tail)#Instance ~> T) {

      def transform[x] = _.fold(
        ifHead = injectHead(_),
        ifTail = injectTail(_)
      )
    }
}

trait LowerPriorityDefaultNaturalTransformations {
  implicit def identityTransform[F[_]]: F ~> F = new (F ~> F) {
    def transform[x] = identity
  }
}
