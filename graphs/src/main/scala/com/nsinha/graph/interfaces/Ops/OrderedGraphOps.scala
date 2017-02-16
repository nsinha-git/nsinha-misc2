package com.nsinha.graph.interfaces.Ops

import com.nsinha.graph.algorithms.Mst.{KruskalMst, PrimsMst}
import com.nsinha.graph.interfaces.Graph.OrderedGraphTrait

/** Created by nsinha on 2/8/17.
  */

trait OrderedGraphOps[A <: Ordered[A]] extends GraphOpsTrait[A] {
  def minSpanningTree(style : String = "kruskal") : Option[OrderedGraphTrait[A]] = {
    style.toLowerCase() match {
      case "kruskal" ⇒
        { new KruskalMst[A](g) }.doMst
      case "prim" ⇒
        { new PrimsMst[A](g) }.doMst
    }
  }
}



