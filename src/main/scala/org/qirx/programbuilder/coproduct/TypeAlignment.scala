package org.qirx.programbuilder
package coproduct

import scala.language.higherKinds

trait TypeAlignment {

  implicit def coproduct[Head[_], Tail[_], T[_]](
    implicit injectHead: Head ~> T,
    injectTail: Tail ~> T) =
    new (Coproduct[Head, Tail]#Instance ~> T) {
      def transform[x] = _.value match {
        case Left(head) => injectHead(head)
        case Right(tail) => injectTail(tail)
      }
    }

}