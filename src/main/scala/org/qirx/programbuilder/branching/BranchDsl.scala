package org.qirx.programbuilder.branching

import org.qirx.programbuilder.Program
import org.qirx.programbuilder.ProgramType

import scala.language.higherKinds

trait BranchDsl {
  implicit class OptionProgramEnhancements[O[_], X, R](right: X)(
    implicit programType: ProgramType[O],
    xAsProgram: X => Program[O]#Instance[Option[R]]) {

    def ifEmpty[L](left: => Program[O]#Instance[L])(
        implicit ev: BranchableProgramType[O, L]) =

      // When the option is empty, use the result of left
      Brancher.branchWithValue(right)(_ toRight left)
  }

  implicit class SeqProgramEnhancements[O[_], X, R](right: X)(
    implicit programType: ProgramType[O],
    xAsProgram: X => Program[O]#Instance[Seq[R]]) {

    def ifEmpty[L](left: => Program[O]#Instance[L])(
      implicit ev: BranchableProgramType[O, L]) =
        
      Brancher.branchWithValue(right) { list =>
        if (list.isEmpty) Left(left)
        else Right(list)
      }
  }
}