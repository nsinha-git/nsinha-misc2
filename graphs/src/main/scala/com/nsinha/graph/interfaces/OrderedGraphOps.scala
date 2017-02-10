package com.nsinha.graph.interfaces

/** Created by nsinha on 2/8/17.
  */
class MinSpanningTree
object KruskalMinSpanningTree extends MinSpanningTree
object PrimsMinSpanningTree extends MinSpanningTree
trait OrderedGraphOps[A <: Ordered[A]] extends GraphOpsTrait[A] {

  def minSpanningTree(g : G, t : MinSpanningTree) : OrderedTreeTrait[A] = ???
}
