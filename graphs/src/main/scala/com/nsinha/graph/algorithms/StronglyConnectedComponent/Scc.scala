package com.nsinha.graph.algorithms.StronglyConnectedComponent

import com.nsinha.graph.algorithms.ConnectedComponent
import com.nsinha.graph.interfaces.Graph.GraphTrait
import com.nsinha.graph.interfaces.Ops.{GraphOpsTrait, TreeOpsTrait}

/** Created by nsinha on 2/7/17.
  */
case class Scc[A](_g : GraphTrait[A]) {
  val gOps = new GraphOpsTrait[A] { override val g = _g }

  def scc() : List[ConnectedComponent[A]] = {
    val allNodesBfs = gOps.bfsTreeAll() map (x ⇒ {
      val treeOps = new TreeOpsTrait[A] {
        override val g = x.graph
        override val tree = x
      }
      (x.rootNode.name → { treeOps.createAPreOrderedList map (_.name) })
    })

    //create a new graph with same nodes names and connect nodes as per allNodesBfs
    //new Node(s"n$i", Math.cos(Math.PI * 2 * i / totalNodes), Math.sin(Math.PI * 2 * i / totalNodes), l map (x ⇒ s"n$x"))

    val allNodes = for (nodeToNeighborMap ← allNodesBfs) yield {
      val correspondingNode = _g.getNode(nodeToNeighborMap._1)
      val nbrs = nodeToNeighborMap._2
      correspondingNode.deepClone(nbrs)
    }
    val newG = _g.deepClone(allNodes)

    val newGops = new GraphOpsTrait[A] { override val g = newG }

    val fc = newGops.getFullyConnectedComponents
    fc
  }

}
