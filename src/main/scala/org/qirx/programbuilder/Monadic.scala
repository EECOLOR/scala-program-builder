package org.qirx.programbuilder

import scala.language.higherKinds

trait Monadic[F[_]] {
  def create[A](a:A):F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}