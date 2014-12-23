package org.qirx.programbuilder

import scala.language.higherKinds

trait NaturalTransformation[-F[_], +G[_]] { fToG =>
  def transform[x]: F[x] => G[x]

  def apply[x](fx: F[x]): G[x] = transform[x](fx)
}