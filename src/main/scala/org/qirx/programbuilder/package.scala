package org.qirx

import scala.language.higherKinds
import org.qirx.programbuilder.branching.BranchDsl

package object programbuilder
  extends NaturalTransformationEnhancements
  with ToProgramImplicits 
  with ProgramEnhancements
  with branching.ProgramEnhancements 
  with branching.BranchDsl {

  type Id[A] = A

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]

  type ::[F[_], T]
  type Nil
  
  def ValueOf[T](value:T) = Return(value)
}