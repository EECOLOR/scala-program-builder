package org.qirx.programbuilder

import scala.language.implicitConversions
import scala.language.higherKinds

trait LowerPriorityToProgramImplicits {
  /**
   * Lifts a container to a program containing only instructions of that type.
   */
  implicit def toProgram[F[_], A](fa: F[A]): Program[F]#Instance[A] =
    Program lift fa
}

trait ToProgramImplicits extends LowerPriorityToProgramImplicits {

  /**
   * Lifts a container to a program of a type captured inside `ProgramType`
   */
  implicit def toComplexProgram[F[_], A, O[_]](fa: F[A])(
    implicit programType: ProgramType[O],
    inject: F ~> O): Program[O]#Instance[A] =
    Program lift inject(fa)

  implicit class ToProgramEnhancement[F[_], A](fa: F[A]) {
    def toProgram[O[_]](implicit programType: ProgramType[O], inject: F ~> O): Program[O]#Instance[A] =
      toComplexProgram(fa)
  }
}