package org.qirx.programbuilder

import scala.language.higherKinds

class Coproduct[F[_], G[_]] {
  case class Instance[A](value: Either[F[A], G[A]])
}

trait CoproductImplicits
  extends coproduct.Injectors
  with coproduct.Removal
  with coproduct.Addition
  with coproduct.TypeAlignment

object Coproduct extends CoproductImplicits {

  def apply[F[_], G[_], A](value: Either[F[A], G[A]]) =
    new Coproduct[F, G].Instance(value)
}