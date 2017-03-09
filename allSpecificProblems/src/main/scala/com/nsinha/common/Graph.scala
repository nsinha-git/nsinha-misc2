package com.nsinha.common

import scala.collection.mutable

/**
  * Created by nsinha on 3/7/17.
  *
  */

case class Coordinate(x: Int, y: Int)

trait Node[A]{
  val name: String
  val children:  mutable.MutableList[A]
}

object Node {
  def processIntoNodesUnDirected[A <: Node[A]](nodes: List[String], _edges: Map[String, List[String]], createNode: String => A): List[A] = {
    var graphMap = mutable.Map[String, A] ()

    for (nodeName <- nodes) yield {
      graphMap += nodeName -> createNode(nodeName)
    }
    for (nodeName <- nodes) yield {
      val curNode = graphMap(nodeName)
      _edges.get(nodeName) match {
        case None =>
        case Some(allEdges) => allEdges map {
          x => curNode.children += graphMap(x)
            graphMap(x).children += curNode
        }
      }
    }
    graphMap.values.toList
  }
}
class UnDirectedGraph[A <: Node[A]](nodeNames: List[String],_edges: Map[String, List[String]]) (createNode: String => A){

  val nodes : List[A] = Node.processIntoNodesUnDirected(nodeNames, _edges, createNode)

  def getChildrenNames(s: String) : List[String] = _edges(s)
  def getParentNames(s: String) = getChildrenNames(s)

  def getChildren(s: String): List[Node[A]] = getChildrenNames(s) flatMap  (x => nodes filter(_.name == x))

  def getParents(s: String): List[Node[A]] = getChildren(s)
}


class DirectedGraph(nodes: List[String],edges: Map[String, List[String]]) {

}
