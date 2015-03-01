package org.qirx.programbuilder

sealed trait HList

case class ::[H, T <: HList](head: H, tail: T) extends HList

sealed trait HNil extends HList

object HNil extends HNil

final case class Contains[L <: HList, X](f: L => X) {
  def apply(l: L): X = f(l)
}

object Contains {
  // Adding this type alias as the return type of the implicits
  // improves performance of compilation

  type Aux[L <: HList, X] = Contains[L, X]

  implicit def inTail[X, H, T <: HList](
    implicit ev: Contains[T, X]): Aux[H :: T, X] =
    Contains[H :: T, X](l => ev(l.tail))

  implicit def atHead[X, T <: HList]: Aux[X :: T, X] =
    Contains[X :: T, X](_.head)
}