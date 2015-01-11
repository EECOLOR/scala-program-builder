package org.qirx.programbuilder

object Test {

  import scala.language.higherKinds

  trait One[T]
  trait Two[T]
  trait Three[T]

  sealed trait Co {
    type Instance[A]
  }
  sealed trait ::[H[_], T <: Co] extends Co
  class AtHead[H[_], T <: Co] extends (H :: T) {
    case class Instance[A](value: H[A])
  }
  class AtTail[H[_], T <: Co] extends (H :: T) {
    case class Instance[A](value: T#Instance[A])
  }
  sealed trait Nil extends Co
  case object Nil extends Nil

  trait ~>[-F[_], +G[_]] {
    def apply[x](f: F[x]): G[x]
  }

  object ~> {
    implicit def atHead[X[_], T <: Co]: X ~> (X :: T)#Instance =
      new (X ~> (X :: T)#Instance) {
        def apply[x](x: X[x]) = new AtHead[X, T].Instance(x)
      }

    implicit def atTail[X[_], H[_], T <: Co](
      implicit xInT: X ~> T#Instance): X ~> (H :: T)#Instance =
      new (X ~> (H :: T)#Instance) {
        def apply[x](x: X[x]) = new AtTail[H, T].Instance(xInT(x))
      }
  }

  type X = One :: Two :: Three :: Nil

  implicitly[One ~> X#Instance]
  implicitly[Two ~> X#Instance]
  implicitly[Three ~> X#Instance]

  case class T1(value:Any) extends One[Boolean]
  case class T2(value:Any) extends One[Int]
  case class T3(value:Any) extends One[String]
  
  case class T4(value:Any) extends Two[Boolean]
  case class T5(value:Any) extends Two[Int]
  case class T6(value:Any) extends Two[String]
  
  case class T7(value:Any) extends Three[Boolean]
  case class T8(value:Any) extends Three[Int]
  case class T9(value:Any) extends Three[String]
  
  type Y = One :: Two :: Three :: Nil
  
  def i[T[_]](t:T[_])(implicit ev:T ~> Y#Instance):T ~> Y#Instance = ev
  i(T1(""))
  i(T2(""))
  i(T3(""))
  i(T4(""))
  i(T5(""))
  i(T6(""))
  i(T7(""))
  i(T8(""))
  i(T9(""))
  
  i(T1(1))
  i(T2(1))
  i(T3(1))
  i(T4(1))
  i(T5(1))
  i(T6(1))
  i(T7(1))
  i(T8(1))
  i(T9(1))
  
  i(T1(true))
  i(T2(true))
  i(T3(true))
  i(T4(true))
  i(T5(true))
  i(T6(true))
  i(T7(true))
  i(T8(true))
  i(T9(true))
  
  i(T1(None))
  i(T2(None))
  i(T3(None))
  i(T4(None))
  i(T5(None))
  i(T6(None))
  i(T7(None))
  i(T8(None))
  i(T9(None))
  
}