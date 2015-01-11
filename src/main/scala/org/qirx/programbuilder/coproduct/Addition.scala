package org.qirx.programbuilder
package coproduct

import scala.language.higherKinds

trait Addition {
  trait TypeBasedPrepend[O[_], T[_]] {
    type Out[_]
  }

  trait LowerPriorityTypeBasedAdd {
    implicit def plain[F[_], G[_]]: TypeBasedPrepend[F, G] {
      // Not sure why, but this construct (instead of placing G directly into
      // the coproduct) prevents a diverging implicit expansion.
    	type T[x] = G[x]
      type Out[x] = (G :+: F :+: CNil)#Instance[x]
    } = null
  }

  object TypeBasedPrepend extends LowerPriorityTypeBasedAdd {

    type Aux[O[_], T[_], R] = TypeBasedPrepend[O, T] { type Out = R }
    
    implicit def coproduct[F[_], G <: Coproduct, H[_]]: TypeBasedPrepend[(F :+: G)#Instance, H] {
      type Out[x] = (H :+: F :+: G)#Instance[x]
    } = null
  }
}