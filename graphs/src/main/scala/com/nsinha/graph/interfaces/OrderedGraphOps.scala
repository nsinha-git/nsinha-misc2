package com.nsinha.graph.interfaces

import com.nsinha.graph.algorithms.Mst.KruskalMst

/** Created by nsinha on 2/8/17.
  */
class MinSpanningTree
object KruskalMinSpanningTree extends MinSpanningTree
object PrimsMinSpanningTree extends MinSpanningTree
trait OrderedGraphOps[A <: Ordered[A]] extends GraphOpsTrait[A] {

  def minSpanningTree() : Option[OrderedGraphTrait[A]] = {
    { new KruskalMst[A](g) }.doMst
  }
}
