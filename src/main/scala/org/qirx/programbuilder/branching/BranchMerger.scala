package org.qirx.programbuilder
package branching

import scala.language.higherKinds

trait MergableProgramType[O[_], T] {
  type Out[_]
  def extractBranch[A](o:O[A]): Either[Branch[T]#Instance[A], Out[A]]
}

object MergableProgramType {
  implicit def programTypeWithBranch[O[_], T](
    implicit typeContainsBranch: Branch[T]#Instance ~> O,
    remover: Coproduct.TypeBasedRemove[O, Branch[T]#Instance]) =
    new MergableProgramType[O, T] {
      type Out[A] = remover.Out[A]
      def extractBranch[A](o:O[A]) = remover remove o
    }
}

object BranchMerger {

  def merge[O[_], T](program:Program[O]#Instance[T])(
      implicit ev:MergableProgramType[O, T]):Program[ev.Out]#Instance[T] = {

    // This is an intermediary type to store the branch. T and A will be 
    // the same (containsMergeableBranch, see above) proves that.  
    type ProgramWithoutBranch[A] = Program[ev.Out]#Instance[Either[T, A]]

    // To turn Free[O, A] into FreeWithoutBranch[A] we need FreeWithoutBranch
    // to have a monad. 
    implicit val withoutBranchMonadic = new Monadic[ProgramWithoutBranch] {
      def create[A](a: A) = Program(Right(a))
      def flatMap[A, B](fa: ProgramWithoutBranch[A])(f: A => ProgramWithoutBranch[B]) =
        fa.flatMap {
          case Right(a) => f(a)
          case Left(t) =>
            Program(Left(t))
        }
    }

    // A way to extract the value of the branch and put it into the new
    // free instance
    val mapper = new (O ~> ProgramWithoutBranch) {
      def transform[A] = o =>
        ev.extractBranch(o) match {
          case Left(a)  => Program(Left(a.value))
          case Right(b) => Program lift b map Right.apply
        }
    }

    // merging now seems simple
    program.runWith(mapper).map(_.merge)
  }
}

