package org.qirx.programbuilder

import scala.language.higherKinds
import scala.language.implicitConversions

trait ProgramEnhancements {

  implicit class ProgramEnhancement[F[_], A](program: Program[F]#Instance[A]) {
    def toProgramType[G[_]](implicit programType: ProgramType[G],
                            fToG: F ~> G): Program[G]#Instance[A] =
      program.adjustType
  }
}