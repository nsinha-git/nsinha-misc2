package com.nsinha.graph.algorithms.Fcc

import com.nsinha.graph.interfaces.NodeTrait

/** Created by nsinha on 2/4/17.
  */

case class ConnectedComponent[A](m : List[NodeTrait]) {
  val nodes = m.foldLeft(Map[String, NodeTrait]()) { (z, el) ⇒
    z + (el.name → el)
  }
  val name = {
    { m map (_.name) }.sorted.mkString("-")
  }
  val nodesNameSet = { m map { _.name } }.toSet

}
