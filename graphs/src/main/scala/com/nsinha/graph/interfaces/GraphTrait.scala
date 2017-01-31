package com.nsinha.graph.interfaces


/**
  * Created by nsinha on 1/27/17.
  */
trait Attribute {
  def name: String
  def value: String
}

trait Weight[A]{
  def getWeight: A
}




trait NodeTrait[A] {
  def children(): List[String]
  def name: String
  def attributes: List[Attribute]
  def setAttribute(attribute: Attribute)
}

class Node[A](nameInit: String, childrenInit: List[String] = Nil, attributesInit: List[Attribute] = Nil) extends NodeTrait[A]
{
  var _children = childrenInit
  var _attributes = attributesInit

  override def setAttribute(attribute: Attribute) = {
    _attributes = _attributes.:+(attribute)
  }
  override def children() = _children

  override def name = nameInit

  override def attributes = _attributes

}

trait EdgeTrait[A] {
  val name: (String, String)
  val isDirected: Boolean
  val weight : Weight[A]
}


trait  GraphTrait[A] {
  def nodes: List[NodeTrait[A]]
  def edges: List[EdgeTrait[A]]
  def checkConsistency: Boolean
  def isDirected: Boolean
}


class Graph[A](_nodes: List[NodeTrait[A]], _isDirected: Boolean, generateWeight:((String, String), Int) => Weight[A]) extends  GraphTrait[A] {
  val _edges = generateEdgesFromNodes(_nodes)
  val nameToNodes : Map[String, NodeTrait[A]] = createNodesMap()
  val nameToEdges : Map[(String, String), EdgeTrait[A]] = createEdgesMap()

  override def nodes = _nodes
  override def edges = _edges

  override def checkConsistency = true
  override def isDirected = _isDirected


  private def generateEdgesFromNodes(nodes: List[NodeTrait[A]]): List[EdgeTrait[A]] = {
    val x = (nodes zip Range(0, nodes.length)) flatMap {
      elem  =>
        val subList : List[EdgeTrait[A]] = elem match {
        case (node, cnt) => val src = node.name
          node.children() map {
            x => new EdgeTrait[A] {
              val name = (src, x)
              val isDirected = _isDirected
              val weight = generateWeight (name, cnt)
            }
          }
        case _ => Nil
      }
       subList
    }

    val y = x.toSet.toList

    //if undirected graph use both ways edges
    if (!_isDirected) {
      val z = y flatMap {
        el => List(el, new EdgeTrait[A] {
          val name = (el.name._2, el.name._1)
          val isDirected = _isDirected
          val weight = el.weight
        })
      }
      z.toSet.toList
    } else y
  }

  private def createNodesMap(): Map[String, NodeTrait[A]] = {
    _nodes.foldLeft(Map[String, NodeTrait[A]]()) { (z,el) => z.+(el.name -> el) }
  }

  private def createEdgesMap(): Map[(String, String), EdgeTrait[A]] = {
    _edges.foldLeft(Map[(String, String), EdgeTrait[A]]()) { (z,el) => z.+(el.name -> el) }
  }

}

trait Tree[A] extends GraphTrait[A] {
  def rootNode: NodeTrait[A]
}

trait OrderedTree[A <: Ordered[A]] extends Tree[A]
trait OrderedGraph[A <: Ordered[A]] extends GraphTrait[A]

trait NumericTree[A <: Numeric[A] with Ordered[A]] extends OrderedTree[A]
trait NumericGraph[A <: Numeric[A] with Ordered[A]] extends OrderedGraph[A]

trait GraphCreateFn[A] {
  def apply(g: GraphTrait[A]) : GraphTrait[A]
}

trait GraphOps[A,G <: GraphTrait[A]]  {
  def bfsTree(name: String): Option[Tree[A]]
  def dfsTree(name: String): Option[Tree[A]]

  def addAttribute(name: String, attribute: Attribute)
  def delAttribute(name: String, attibute: String)

  def isFullyConnected: Boolean
  def getStronglyConnectedComponenets: List[GraphTrait[A]]
  def getWeaklyConnectedComponents: List[GraphTrait[A]]

  def topologicalSort: List[Tree[A]]

  def createNewGraph(fn: GraphCreateFn[A]): GraphTrait[A]
}


trait OrderedGraphOps[A <: Ordered[A]] extends GraphOps[A, OrderedGraph[A]]{
  def minSpanningTree(): OrderedTree[A]
}

trait NumericGrapOps[A <: Numeric[A] with Ordered[A]] extends  OrderedGraphOps[A] {
  def findMinDistSrcDest(src: String, dest: String): Option[A]
  def findMinDistForEachSrcDest(): List[(String, String, Option[A])]
  def findMaxFlowSrcDest(src: String, dest: String): A
  def findMaxFlowForEachSrcDest(): List[(String, String, A)]
}







