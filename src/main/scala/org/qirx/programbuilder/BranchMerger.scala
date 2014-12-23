package org.qirx.programbuilder

import scala.language.higherKinds

trait BranchMerger[O[_], A] {
  type Out[_]
  def merge(program: Program[O]#Instance[A]): Program[Out]#Instance[A]
}

object BranchMerger {

  implicit def merger[O[_], T](
    implicit containsMergableBranch: Branch[T]#Instance ~> O,
    result: O - Branch[T]#Instance) = {

    // This is an intermediary type to store the branch. T and A will be 
    // the same (containsMergeableBranch, see above) proves that.  
    type ProgramWithoutBranch[A] = Program[result.Out]#Instance[Either[T, A]]

    // To turn Free[O, A] into FreeWithoutBranch[A] we need FreeWithoutBranch
    // to have a monad. 
    implicit val withoutWithoutBranchMonadic = new Monadic[ProgramWithoutBranch] {
      def create[A](a: A) = Program(Right(a))
      def flatMap[A, B](fa: Program[result.Out]#Instance[Either[T, A]])(f: A => Program[result.Out]#Instance[Either[T, B]]) =
        fa.flatMap {
          case Right(a) => f(a)
          case Left(t)  =>
            Program(Left(t))
        }
    }

    // A way to extract the value of the branch and put it into the new
    // free instance
    val mapper = new (O ~> ProgramWithoutBranch) {
      def transform[A] = o =>
        result.removeFrom(o) match {
          case Left(a)  => Program(Left(a.value))
          case Right(b) => Program lift b map Right.apply
        }
    }

    // merging now seems simple
    new BranchMerger[O, T] {
      type Out[x] = result.Out[x]
      def merge(program: Program[O]#Instance[T]): Program[Out]#Instance[T] =
        program.runWith(mapper).map(_.merge)
    }
  }
}