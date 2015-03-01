package org.qirx.programbuilder

import scala.language.higherKinds

sealed trait Coproduct {
  type Instance[x]
  type Injectors[O[_]] <: HList
}
sealed trait :+:[H[_], T <: Coproduct] extends Coproduct {
  type Instance[x] <: Coproduct.Ops[H, T, x]
	type Injectors[O[_]] = (H ~> O) :: T#Injectors[O]
}
sealed trait CNil extends Coproduct {
  type Injectors[O[_]] = HNil
}

trait CoproductImplicits extends coproduct.Removal with coproduct.Addition

object Coproduct extends CoproductImplicits {

  trait Ops[H[_], T <: Coproduct, x] {
  
  def fold[X](ifHead: H[x] => X, ifTail: T#Instance[x] => X): X
  
  def toEither:Either[H[x], T#Instance[x]] = 
    fold(ifHead = Left(_), ifTail = Right(_))
}
  
  class Head[H[_], T <: Coproduct] extends (H :+: T) {
    case class Instance[x](head: H[x]) extends Ops[H, T, x] {
      def fold[X](ifHead: H[x] => X, ifTail: T#Instance[x] => X): X =
        ifHead(head)
    }
  }
  object Head {
    def apply[H[_], T <: Coproduct, A](head: H[A]): (H :+: T)#Instance[A] =
      new Head[H, T].Instance(head)
  }

  class Tail[H[_], T <: Coproduct] extends (H :+: T) {
    case class Instance[x](tail: T#Instance[x]) extends Ops[H, T, x] {
      def fold[X](ifHead: H[x] => X, ifTail: T#Instance[x] => X): X =
        ifTail(tail)
    }
  }
  object Tail {
    def apply[H[_], T <: Coproduct, A](tail: T#Instance[A]): (H :+: T)#Instance[A] =
      new Tail[H, T].Instance(tail)
  }
}