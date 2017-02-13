package com.nsinha.graph.algorithms.CommonAncestor

/** Created by nsinha on 2/12/17.
  */
class CommonAncestor {

  val g = new Graph {
    override var setOfTrees : Set[Tree] = Set()
    override var mapOfNodes : Map[String, Node] = Map()
  }

  def checkCycleOK(src : String, dest : String) : Boolean = {
    val ret = g.addANode(src, dest)
    ret match {
      case None       ⇒ false
      case Some(tree) ⇒ true
    }
  }
}
