package com.nsinha.graph.interfaces.Ops

import com.nsinha.graph.interfaces.Common._
import com.nsinha.graph.interfaces.Graph.NodeTrait
import com.nsinha.graph.interfaces._

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
