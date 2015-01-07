package org.qirx.programbuilder.branching

import org.qirx.programbuilder.Program
import org.qirx.programbuilder.ProgramType

import scala.language.higherKinds

trait BranchDsl {
  implicit class OptionProgramEnhancements[O[_], X, R](right: X)(
    implicit programType: ProgramType[O],
    xAsProgram: X => Program[O]#Instance[Option[R]]) {

    def ifNone[L](left: => Program[O]#Instance[L])(
      implicit ev: BranchableProgramType[O, L]) =

      Brancher.branchWithValue(right)(_ toRight left)

    def ifSome[L](left: => Program[O]#Instance[L])(
      implicit ev: BranchableProgramType[O, L]) =

      Brancher.branchWithValue(right) { o =>
        if (o.isDefined) Left(left)
        else Right(())
      }

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

    def ifNonEmpty[L](left: => Program[O]#Instance[L])(
      implicit ev: BranchableProgramType[O, L]) =

      Brancher.branchWithValue(right) { list =>
        if (list.nonEmpty) Left(left)
        else Right(())
      }
  }

  implicit class BooleanProgramEnhancements[O[_], X](right: X)(
    implicit programType: ProgramType[O],
    xAsProgram: X => Program[O]#Instance[Boolean]) {

    def ifFalse[L](left: => Program[O]#Instance[L])(
      implicit ev: BranchableProgramType[O, L]) =

      Brancher.branchWithValue(right) {
        case true  => Right(())
        case false => Left(left)
      }
  }
}