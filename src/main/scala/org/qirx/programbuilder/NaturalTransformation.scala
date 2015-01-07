package org.qirx.programbuilder

import scala.language.higherKinds
import scala.concurrent.Future

trait NaturalTransformation[-F[_], +G[_]] { fToG =>
  def transform[x]: F[x] => G[x]

  def apply[x](fx: F[x]): G[x] = transform[x](fx)
}

object NaturalTransformation extends DefaultNaturalTransformations

trait DefaultNaturalTransformations {

  implicit def identityTransform[F[_]]:F ~> F = new (F ~> F) {
    def transform[x] = identity
  }

  implicit object IdToFuture extends (Id ~> Future) {
    def transform[x] = Future successful _
  }

  implicit def idToEither[L]:Id ~> ({ type T[x] = Either[L, x] })#T =
    new (Id ~> ({ type T[x] = Either[L, x] })#T) {
      def transform[x] = Right apply _
    }

  implicit object OptionToSeq extends (Option ~> Seq) {
    def transform[x] = _.toSeq
  }
}