package org.qirx.programbuilder

import scala.language.higherKinds
import org.qirx.programbuilder.branching.Branch

final class ProgramType[F[_]] {
  type Out[x] = F[x]
  
  def withBranch[T](implicit prepend: Coproduct.TypeBasedPrepend[F, Branch[T]#Instance]) =
    new ProgramType[prepend.Out]
}

object ProgramType {
  def apply[T <: Coproduct] = new ProgramType[T#Instance]
}