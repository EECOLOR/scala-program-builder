package org.qirx.programbuilder
package branching

import scala.language.higherKinds

trait BranchableProgramType[O[_], A] {
  def asBranch[B](program: Program[O]#Instance[A]): Program[O]#Instance[B]
}

object BranchableProgramType {
  implicit def programTypeWithBranch[O[_], A](
    implicit injectBranch: Branch[A]#Instance ~> O) =
    new BranchableProgramType[O, A] {
      def asBranch[B](program: Program[O]#Instance[A]): Program[O]#Instance[B] =
        program flatMap (t => injectBranch(Branch[A, B](t)))
    }
}

object Brancher {

  def branchWithProgram[O[_], T, A, B](
    program: Program[O]#Instance[A])(
      branch: A => Either[Program[O]#Instance[T], Program[O]#Instance[B]])(
        implicit ev: BranchableProgramType[O, T]): Program[O]#Instance[B] =
    program flatMap { a =>
      branch(a) match {
        case Left(branch)   => ev asBranch branch
        case Right(program) => program
      }
    }

  def branchWithValue[O[_], T, A, B](
    program: Program[O]#Instance[A])(
      branch: A => Either[Program[O]#Instance[T], B])(
        implicit ev: BranchableProgramType[O, T]): Program[O]#Instance[B] =
    branchWithProgram(program)(branch andThen (_.right map Program[O, B]))

  /*
     * This looks like a scary method, and it probably is. It uses the A value in F[A]
     * to determine if it needs to branch. If it needs to branch it will secretly
     * (using the Branch[L]#Instance[R] type) insert the branch based on the L value 
     * in G[L].
     * 
     * It becomes a bit complicated because we do not know anything about F[_] and 
     * G[_], to get at the values that are inside of them we need to wrap them in 
     * Free. In order to wrap them in Free we need to know a few things:
     * - The program type O[_]
     * - A way to inject F into O
     * - A way to inject G into O
     * 
     * Last but not least we need to inject the branch into O
     */
}