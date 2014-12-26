package org.qirx.programbuilder
package coproduct

import scala.language.higherKinds

trait Injectors {
  implicit def atCoproductHead[Head[_], Tail[_]] =
    new (Head ~> Coproduct[Head, Tail]#Instance) {
      def transform[A] = fa => Coproduct(Left(fa))
    }

  implicit def inCoproductTail[F[_], Head[_], Tail[_]](
    implicit injectInTail: F ~> Tail) =
    new (F ~> Coproduct[Head, Tail]#Instance) {
      def transform[A] = fa => Coproduct(Right(injectInTail(fa)))
    }
}
