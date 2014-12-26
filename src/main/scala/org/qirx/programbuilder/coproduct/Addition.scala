package org.qirx.programbuilder
package coproduct

import scala.language.higherKinds

trait Addition {
  trait TypeBasedAdd[O[_], T[_]] {
    type Out[_]
  }

  trait LowerPriorityTypeBasedAdd {
    implicit def plain[F[_], G[_]]: TypeBasedAdd[F, G] {
      // Not sure why, but this construct (instead of placing G directly into
      // the coproduct) prevents a diverging implicit expansion.
    	type T[x] = G[x]
      type Out[x] = Coproduct[F, T]#Instance[x]
    } = null
  }

  object TypeBasedAdd extends LowerPriorityTypeBasedAdd {

    implicit def coproduct[F[_], G[_], H[_]](
      implicit add: TypeBasedAdd[G, H]): TypeBasedAdd[Coproduct[F, G]#Instance, H] {
      type Out[x] = Coproduct[F, add.Out]#Instance[x]
    } = null
  }
}