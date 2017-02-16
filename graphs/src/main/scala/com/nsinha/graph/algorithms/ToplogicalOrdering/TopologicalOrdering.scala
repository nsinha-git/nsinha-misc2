package com.nsinha.graph.algorithms.ToplogicalOrdering

import com.nsinha.graph.interfaces.Graph.GraphTrait
import com.nsinha.graph.interfaces.Ops.{GraphOpsTrait, TreeOpsTrait}
import com.nsinha.graph.interfaces._

import scala.collection.mutable

/** Created by nsinha on 2/8/17.
  */
class TopologicalOrdering[A](_g : GraphTrait[A]) {
  val gOps : GraphOpsTrait[A] = new GraphOpsTrait[A] {
    override val g : G = _g
  }

  def topoOrder() : List[String] = {
    val nodes = _g.nodes
    val visited = new mutable.Queue[String]()
    val l = new mutable.Queue[String]()
    if (gOps.doesHaveCycles()) {
      Nil
    }
    else {
      for (node ← nodes) {
        val dfs = gOps.dfsTree(node.name)
        dfs match {
          case None ⇒
          case Some(treeTrait) ⇒
            val treeOps = new TreeOpsTrait[A] {
              override val tree = treeTrait
              override val g = treeTrait.graph
            }
            treeOps.createAPostOrderedList foreach { x ⇒
              if (!visited.contains(x.name)) {
                visited.enqueue(x.name)
                l.enqueue(x.name)
              }
            }
        }
      }

      l.toList
    }
  }
}
