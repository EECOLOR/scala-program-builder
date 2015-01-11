package org.qirx.programbuilder

import scala.language.higherKinds

class Coproduct[F[_], G[_]] {
  case class Instance[A](value: Either[F[A], G[A]])
}

trait CoproductImplicits extends coproduct.Removal with coproduct.Addition

object Coproduct extends CoproductImplicits {

  def apply[F[_], G[_], A](value: Either[F[A], G[A]]) =
    new Coproduct[F, G].Instance(value)
}