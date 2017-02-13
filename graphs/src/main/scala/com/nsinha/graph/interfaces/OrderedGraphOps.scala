package com.nsinha.graph.interfaces

import com.nsinha.graph.algorithms.Mst.{KruskalMst, PrimsMst}

/** Created by nsinha on 2/8/17.
  */
class MinSpanningTree
object KruskalMinSpanningTree extends MinSpanningTree
object PrimsMinSpanningTree extends MinSpanningTree
trait OrderedGraphOps[A <: Ordered[A]] extends GraphOpsTrait[A] {

  def minSpanningTree(style: String = "kruskal") : Option[OrderedGraphTrait[A]] = {
    style.toLowerCase() match {
      case "kruskal" =>
        { new KruskalMst[A](g) }.doMst
      case "prim" =>
        { new PrimsMst[A](g) }.doMst
    }
  }
}
