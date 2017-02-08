package com.nsinha.graph.interfaces

/** Created by nsinha on 2/1/17.
  */

class Graph[A](_nodes : List[NodeTrait], _isDirected : Boolean, generateWeight : ((String, String), Int) ⇒ Weight[A]) extends GraphTrait[A] {
  val _edges = generateEdgesFromNodes(_nodes)
  val nameToNodes : Map[String, NodeTrait] = createNodesMap()
  val (srcDestToEdges : Map[(String, String), EdgeTrait[A]], srcToEdges : Map[String, List[EdgeTrait[A]]], destToEdges : Map[String, List[EdgeTrait[A]]]) = createEdgesMap()

  override val nodes = _nodes
  override val edges = _edges

  override def getWeightFn = generateWeight

  override def checkConsistency = true

  override def isDirected = _isDirected

  override def deepClone() = {
    new Graph[A](_nodes, _isDirected, generateWeight)
  }

  override def deepClone(localNodes : List[NodeTrait]) = {
    new Graph[A](localNodes, _isDirected, generateWeight)
  }

  override def getEdge(n : NodeTrait) : List[EdgeTrait[A]] = {
    getEdgeWithSrc(n.name) ++ getEdgeWithDest(n.name)
  }

  override def getEdgeWithSrc(n : String) : List[EdgeTrait[A]] = {
    srcToEdges.get(n).foldLeft(List[EdgeTrait[A]]()) { (z, x) ⇒ x }
  }

  override def getEdgeWithDest(n : String) : List[EdgeTrait[A]] = {
    destToEdges.get(n).foldLeft(List[EdgeTrait[A]]()) { (z, x) ⇒ x }
  }

  override def getNode(n : String) : NodeTrait = {
    nameToNodes(n)
  }

  private def generateEdgesFromNodes(nodes : List[NodeTrait]) : List[EdgeTrait[A]] = {
    val x = (nodes zip Range(0, nodes.length)) flatMap {
      elem ⇒
        val subList : List[EdgeTrait[A]] = elem match {
          case (node, cnt) ⇒
            val src = node.name
            node.children() map {
              x ⇒
                new EdgeTrait[A] {
                  val name = (src, x)
                  val isDirected = _isDirected
                  val weight = generateWeight(name, cnt)
                }
            }
          case _ ⇒ Nil
        }
        subList
    }

    val y = x.toSet.toList

    //if undirected graph use both ways edges
    if (!_isDirected) {
      val z = y flatMap {
        el ⇒
          List(el, new EdgeTrait[A] {
            val name = (el.name._2, el.name._1)
            val isDirected = _isDirected
            val weight = el.weight
          })
      }
      z.toSet.toList
    }
    else y
  }

  private def createNodesMap() : Map[String, NodeTrait] = {
    _nodes.foldLeft(Map[String, NodeTrait]()) { (z, el) ⇒ z.+(el.name → el) }
  }

  private def createEdgesMap() : (Map[(String, String), EdgeTrait[A]], Map[String, List[EdgeTrait[A]]], Map[String, List[EdgeTrait[A]]]) = {
    (
      _edges.foldLeft(Map[(String, String), EdgeTrait[A]]()) { (z, el) ⇒ z.+(el.name → el) },
      _edges.foldLeft(Map[String, List[EdgeTrait[A]]]()) { (z, el) ⇒
        z.get(el.name._1) match {
          case Some(x) ⇒ z.+(el.name._1 → x.+:(el))
          case None    ⇒ z.+(el.name._1 → List(el))
        }
      },
      _edges.foldLeft(Map[String, List[EdgeTrait[A]]]()) { (z, el) ⇒
        z.get(el.name._2) match {
          case Some(x) ⇒ z.+(el.name._2 → x.+:(el))
          case None    ⇒ z.+(el.name._2 → List(el))
        }
      }
    )
  }
}


