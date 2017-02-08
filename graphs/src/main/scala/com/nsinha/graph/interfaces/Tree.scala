package com.nsinha.graph.interfaces

/** Created by nsinha on 2/7/17.
  */
class Tree[A](_rootNode : NodeTrait, g : GraphTrait[A]) extends TreeTrait[A] {
  override def rootNode = _rootNode

  override def graph = g

  override val nodes : List[NodeTrait] = g.nodes
  override val edges : List[EdgeTrait[A]] = g.edges

  override def checkConsistency : Boolean = g.checkConsistency

  override def getEdge(n : NodeTrait) : List[EdgeTrait[A]] = g.getEdge(n)

  override def getEdgeWithSrc(n : String) : List[EdgeTrait[A]] = g.getEdgeWithSrc(n)

  override def getEdgeWithDest(n : String) : List[EdgeTrait[A]] = g.getEdgeWithDest(n)

  override def getNode(n : String) : NodeTrait = g.getNode(n)

  override def isDirected : Boolean = g.isDirected

  override def getWeightFn = g.getWeightFn

  override def deepClone() : TreeTrait[A] = new Tree(_rootNode.deepClone(), g.deepClone())

  override def deepClone(n : List[NodeTrait]) : TreeTrait[A] = ???
}
