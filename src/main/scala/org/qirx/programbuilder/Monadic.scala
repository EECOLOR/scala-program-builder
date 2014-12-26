package org.qirx.programbuilder

import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.collection.GenTraversableOnce

trait Monadic[F[_]] {
  def create[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monadic extends DefaultMonadics

trait DefaultMonadics {

  implicit object IdMonadic extends Monadic[Id] {
    def create[A](a: A): Id[A] = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  implicit def futureMonadic(implicit ec: ExecutionContext) =
    new Monadic[Future] {
      def create[A](a: A): Future[A] = Future successful a
      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        fa flatMap f
    }

  private type RightBiased[L] = {
    type ScalaEither[L, R] = scala.Either[L, R]
    type Either[x] = ScalaEither[L, x]
  }

  implicit def eitherMonadic[L] =
    new Monadic[RightBiased[L]#Either] {
      def create[A](a: A) = Right(a)
      def flatMap[A, B](fa: RightBiased[L]#Either[A])(f: A => RightBiased[L]#Either[B]) =
        fa.right flatMap f
    }

  implicit object SeqMonadic extends Monadic[Seq] {
      def create[A](a: A): Seq[A] = Seq(a)
      def flatMap[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] =
        fa flatMap f
    }
}