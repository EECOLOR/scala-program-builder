package org.qirx.programbuilder
package coproduct

import scala.language.higherKinds

trait Removal { self: Addition =>

  /**
   * Type to represent removal
   */
  trait TypeBasedRemove[O[_], T[_]] {

    
    /**
     * The result type after removal
     */
	  type Result <: Coproduct

    /**
     * Removes the value of type T from the given O
     *
     * Returns either the removed value T or the result if T
     * was not in there as a value.
     *
     * For example, if `O[A]` is `Coproduct[Option, List]#Instance[A]` and
     * `T[A]` is `List[A]`, it will return `Left(Option(...))` if `List` was
     * not in there or `Right(List(...))` if it turned out to be a `List`.
     *
     * A more complex example.
     *
     * `O` is `Coproduct[Option, Coproduct[List, Seq]]`
     * `T` is `List`
     * `Out` will be `Coproduct[Option, Seq]`
     * result will be `Either[List[A], Coproduct[Option, Seq]]`
     */
    def remove[A]: O[A] => Either[T[A], Result#Instance[A]]
  }

  object TypeBasedRemove {

    // Simple alias to allow the fancy O - T notation
    private type -[O[_], T[_]] = TypeBasedRemove[O, T]

    implicit def atHead[T[_], Tail <: Coproduct] =
      new ((T :+: Tail)#Instance - T) {
        type Result = Tail
        def remove[A] = _.toEither
      }

    implicit def inTail[T[_], Head[_], Tail <: Coproduct](
      implicit tailRemover: Tail#Instance - T) =
      new ((Head :+: Tail)#Instance - T) {

        type Result = (Head :+: tailRemover.Result)

        def remove[A] =
          _.toEither match {
            case Left(a) =>
              Right(Coproduct.Head(a))
            case Right(b) =>
              tailRemover.remove(b) match {
                case Left(a)  => Left(a)
                case Right(b) => Right(Coproduct.Tail(b))
              }
          }
      }
  }
}