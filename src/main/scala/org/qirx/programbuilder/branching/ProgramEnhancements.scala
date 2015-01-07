package org.qirx.programbuilder
package branching

import scala.language.higherKinds

trait ProgramEnhancements {
  implicit class EnhanceProgramWithMergeBranch[O[_], A](program: Program[O]#Instance[A]) {
    def mergeBranch(implicit ev: MergableProgramType[O, A]):Program[ev.Out]#Instance[A] =
      BranchMerger merge program
  }
}