package com.nsinha.common

import scala.collection.mutable

/** Created by nsinha on 3/7/17.
  *
  */
case class Coordinate(x : Int, y : Int) extends Ordering[Coordinate] {
  override def hashCode() : Int = x.hashCode() + y.hashCode()

  override def compare(a : Coordinate, b : Coordinate) : Int = {
    import StandardBooleanCoercions._
    val r1 = implicitly[Ordering[Int]].compare(a.x, b.x)
    val r2 = implicitly[Ordering[Int]].compare(a.y, b.y)
    if (r1) {
      r1
    }
    else r2
  }
}

trait Node[A] {
  val name : String
  val children : mutable.MutableList[A]
  val parents : mutable.MutableList[A]
  var weightMaps = mutable.HashMap[String, Int]()
}

object Node {
  def processIntoNodesDirectedWithWeights[A <: Node[A]](nodes : List[String], _edges : Map[String, List[(String, Int)]], createNode : String ⇒ A) : List[A] = {
    var graphMap = mutable.HashMap[String, A] ()
    var graphMapForWeights = mutable.Map[String, mutable.HashMap[String, Int]] ()

    for (nodeName ← nodes) yield {
      graphMap += nodeName → createNode(nodeName)
      graphMapForWeights += nodeName → mutable.HashMap[String, Int]()
    }
    for (nodeName ← nodes) yield {
      val curNode = graphMap(nodeName)
      _edges.get(nodeName) match {
        case None ⇒
        case Some(allEdges) ⇒ allEdges map {
          x ⇒
            curNode.children += graphMap(x._1)
            graphMap(x._1).parents += curNode
            graphMapForWeights(curNode.name) += (x)
        }
      }
    }

    for (nodeName ← nodes) yield {
      val node = graphMap(nodeName)
      graphMapForWeights(nodeName) map { x ⇒ node.weightMaps += x }
    }

    graphMap.values.toList
  }

  def processIntoNodesUnDirectedWithWeights[A <: Node[A]](nodes : List[String], _edges : Map[String, List[(String, Int)]], createNode : String ⇒ A) : List[A] = {
    var graphMap = mutable.HashMap[String, A] ()
    var graphMapForWeights = mutable.Map[String, mutable.HashMap[String, Int]] ()

    for (nodeName ← nodes) yield {
      graphMap += nodeName → createNode(nodeName)
      graphMapForWeights += nodeName → mutable.HashMap[String, Int]()
    }
    for (nodeName ← nodes) yield {
      val curNode = graphMap(nodeName)
      _edges.get(nodeName) match {
        case None ⇒
        case Some(allEdges) ⇒ allEdges map {
          x ⇒
            curNode.children += graphMap(x._1)
            curNode.parents += graphMap(x._1)
            graphMap(x._1).parents += curNode
            graphMap(x._1).children += curNode

            graphMapForWeights(curNode.name) += (x)
            graphMapForWeights(x._1) += (curNode.name → x._2)
        }
      }
    }

    for (nodeName ← nodes) yield {
      val node = graphMap(nodeName)
      graphMapForWeights(nodeName) map { x ⇒ node.weightMaps += x }
    }

    graphMap.values.toList
  }

  def processIntoNodesDirected[A <: Node[A]](nodes : List[String], _edges : Map[String, List[String]], createNode : String ⇒ A) : List[A] = {
    var graphMap = mutable.Map[String, A] ()

    for (nodeName ← nodes) yield {
      graphMap += nodeName → createNode(nodeName)
    }
    for (nodeName ← nodes) yield {
      val curNode = graphMap(nodeName)
      _edges.get(nodeName) match {
        case None ⇒
        case Some(allEdges) ⇒ allEdges map {
          x ⇒
            curNode.children += graphMap(x)
            graphMap(x).parents += curNode
        }
      }
    }
    graphMap.values.toList
  }

  def processIntoNodesUnDirected[A <: Node[A]](nodes : List[String], _edges : Map[String, List[String]], createNode : String ⇒ A) : List[A] = {
    var graphMap = mutable.Map[String, A] ()

    for (nodeName ← nodes) yield {
      graphMap += nodeName → createNode(nodeName)
    }
    for (nodeName ← nodes) yield {
      val curNode = graphMap(nodeName)
      _edges.get(nodeName) match {
        case None ⇒
        case Some(allEdges) ⇒ allEdges map {
          x ⇒
            curNode.children += graphMap(x)
            graphMap(x).children += curNode

            curNode.parents += graphMap(x)
            graphMap(x).parents += curNode
        }
      }
    }
    graphMap.values.toList
  }
}

trait GraphTrait[A <: Node[A]] {
  val nodes : List[A]
  val mapOfNamesToNodes : Map[String, A]

  def getChildrenNames(s : String) : List[String] = { mapOfNamesToNodes(s).children map (_.name) }.toList
  def getParentNames(s : String) = { mapOfNamesToNodes(s).parents map (_.name) }.toList

  def getChildren(s : String) : List[Node[A]] = { mapOfNamesToNodes(s).children }.toList

  def getParents(s : String) : List[Node[A]] = { mapOfNamesToNodes(s).parents }.toList
}
class UnDirectedGraph[A <: Node[A]](nodeNames : List[String], _edges : Map[String, List[String]])(createNode : String ⇒ A) extends GraphTrait[A] {
  override val nodes : List[A] = Node.processIntoNodesUnDirected(nodeNames, _edges, createNode)
  override val mapOfNamesToNodes : Map[String, A] = { nodes map { x ⇒ x.name → x } } toMap
}

class DirectedGraph[A <: Node[A]](nodeNames : List[String], _edges : Map[String, List[String]])(createNode : String ⇒ A) extends UnDirectedGraph[A](nodeNames, _edges)(createNode) with GraphTrait[A] {
  override val nodes : List[A] = Node.processIntoNodesDirected(nodeNames, _edges, createNode)
}

class UnDirectedGraphWithEdgeWeights[A <: Node[A]](nodesNames : List[String], _edges : Map[String, List[(String, Int)]])(createNode : String ⇒ A) extends GraphTrait[A] {

  override val nodes : List[A] = Node.processIntoNodesUnDirectedWithWeights(nodesNames, _edges, createNode)
  override val mapOfNamesToNodes : Map[String, A] = { nodes map { x ⇒ x.name → x } } toMap
}

abstract class DirectedGraphWithEdgeWeights[A <: Node[A]](nodesNames : List[String], _edges : Map[String, List[(String, Int)]])(createNode : String ⇒ A) extends GraphTrait[A] {

  override val nodes : List[A] = Node.processIntoNodesUnDirectedWithWeights(nodesNames, _edges, createNode)
}
