package org.qirx.programbuilder

import scala.language.higherKinds

// Type to represent a removal. We remove T from O
trait Remove[O[_], T[_]] {
  type Out[_]

  // If T was in there, we remove it, otherwise we return the result
  def removeFrom[A]: O[A] => Either[T[A], Out[A]]
}

object Remove {

  implicit def atHead[T[_], Tail[_]] =
    new Remove[Coproduct[T, Tail]#Instance, T] {
      type Out[x] = Tail[x]
      def removeFrom[A] = _.value
    }

  implicit def atTail[T[_], Head[_]] =
    new Remove[Coproduct[Head, T]#Instance, T] {
      type Out[x] = Head[x]
      def removeFrom[A] = _.value.swap
    }

  implicit def inTail[T[_], Head[_], Tail[_]](
    implicit resultType: Tail - T) =
    new Remove[Coproduct[Head, Tail]#Instance, T] {

      type Out[x] = Coproduct[Head, resultType.Out]#Instance[x]

      def removeFrom[A] =
        _.value match {
          case Left(a) =>
            Right(Coproduct(Left(a)))
          case Right(b) =>
            resultType.removeFrom(b) match {
              case Left(a)  => Left(a)
              case Right(b) => Right(Coproduct(Right(b)))
            }
        }
    }
}