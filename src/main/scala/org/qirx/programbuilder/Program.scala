package org.qirx.programbuilder

import scala.language.higherKinds

class Program[F[_]] {

  trait Instance[+A] {

    def flatMap[B](f: A => Program[F]#Instance[B]): Program[F]#Instance[B] =
      this match {
        case Create(a)      => f(a)
        case FlatMap(fa, g) => FlatMap(fa, g andThen (_ flatMap f))
      }

    def map[B](f: A => B): Program[F]#Instance[B] =
      flatMap(f andThen Create.apply)

    def transformType[G[_]](implicit fToG: F ~> G): Program[G]#Instance[A] =
      this match {
        case Create(a)      => Program(a)
        case FlatMap(fa, f) => Program lift fToG(fa) flatMap (f andThen (_ transformType fToG))
      }

    def runWith[G[_], AA >: A](runner: F ~> G)(implicit G: Monadic[G]): G[AA] =
      this match {
        case Create(a) => G create a
        case FlatMap(fa, f) =>
          val ga = runner transform fa
          G.flatMap(ga)(a => f(a) runWith runner)
      }

    def run(implicit runner: F ~> Id): A = this runWith runner
  }

  private case class Create[A](a: A) extends Instance[A]

  private case class FlatMap[A, B](fa: F[A], f: A => Program[F]#Instance[B]) extends Instance[B]
}

object Program {
  def apply[F[_], A](a: A): Program[F]#Instance[A] =
    new Program[F].Create(a)

  def lift[F[_], A](fa: F[A]): Program[F]#Instance[A] =
    new Program[F].FlatMap(fa, Program.apply[F, A])
    
}