package org.qirx.programbuilder

import scala.language.higherKinds

import scala.language.implicitConversions

// These can not be defined directly on NaturalTransformation because of
// variance notation

trait NaturalTransformationEnhancements extends LowerPriorityNaturalTransformationEnhancements {

  implicit class EnhanceNaturalTransformation[F[_], O[_]](fToO: F ~> O) {

    def andThen[G[_]](oToG: O ~> G): F ~> G =
      new (F ~> G) {
        def transform[x] = fa => oToG(fToO(fa))
      }

    def autoAdjust[G[_]](implicit gToF: G ~> F): G ~> O = gToF andThen fToO
  }

  implicit class EnhanceCoproductNaturalTransformation[H[_], T <: Coproduct, O[_]](
    fToO: (H :+: T)#Instance ~> O) {

    def :+:[G[_]](gToO: G ~> O): (G :+: H :+: T)#Instance ~> O =
      new ((G :+: H :+: T)#Instance ~> O) {
        def transform[x] = _.fold(
          ifHead = gToO(_),
          ifTail = fToO(_)
        )
      }
  }
}

trait LowerPriorityNaturalTransformationEnhancements {

  implicit class EnhanceNonCoproductNaturalTransformation[F[_], O[_]](
    fToO: F ~> O) {

    def :+:[G[_]](gToO: G ~> O): (G :+: F :+: CNil)#Instance ~> O =
      new ((G :+: F :+: CNil)#Instance ~> O) {
        def transform[x] = _.fold(
          ifHead = gToO(_),
          ifTail = _.fold(
            ifHead = fToO(_),
            ifTail = _ => sys.error("I don't know how you did it, but cheers: you created a CNil instance")
          )
        )
      }
  }
}
