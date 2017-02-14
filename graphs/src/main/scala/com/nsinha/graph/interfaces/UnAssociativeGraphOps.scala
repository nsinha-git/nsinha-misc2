package com.nsinha.graph.interfaces

/**
  * Created by nsinha on 2/13/17.
  */
trait UnAssociativeGraphOps[A <: UnAssociativeGroupElem] { }

trait AssociativeGraphOps[A <: AssociativeGroupElem] {
  def shortestPath(x: NodeTrait, y: NodeTrait): A
}

trait UnAssociativeNonDistributiveRingGraphOps[A <: UnAssociativeNonDistributiveRingElem] { }

trait AssociativeNonDistributiveRingGraphOps[A <: AssociativeNonDistributiveRingElem] { }

trait AssociativeDistributiveRingGraphOps[A <: AssociativeDistributiveRingElem] {
  def shortestPath(x: NodeTrait, y: NodeTrait): A
}
