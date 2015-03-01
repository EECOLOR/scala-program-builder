package org.qirx

import scala.language.higherKinds
import org.qirx.programbuilder.branching.Branch

package object programbuilder
  extends NaturalTransformationEnhancements
  with ToProgramImplicits
  with ProgramEnhancements
  with branching.ProgramEnhancements
  with branching.BranchDsl {

  type Id[A] = A

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]

  def ValueOf[T](value: T) = Return(value)

  type WithBranch[W <: ProgramInformation, T] = W#WithBranch[T]
}