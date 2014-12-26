package org.qirx.programbuilder

import scala.language.higherKinds

import scala.language.implicitConversions

trait NaturalTransformationEnhancements {

  // These can not be defined directly on NaturalTransformation because of 
  // variance notation
  implicit class EnhanceNaturalTransformation[F[_], O[_]](fToO: F ~> O) {

    def or[G[_]](gToO: G ~> O) =
      new (Coproduct[G, F]#Instance ~> O) {
        def transform[x] = _.value match {
          case Left(g)  => gToO(g)
          case Right(f) => fToO(f)
        }
      }

    def andThen[G[_]](oToG: O ~> G) =
      new (F ~> G) {
        def transform[x] = fa => oToG(fToO(fa))
      }
  }

  implicit def adjustRunner[I[_], O[_], T[_]](runner: I ~> T)(
    implicit oToI: O ~> I): O ~> T =
    oToI andThen runner
}