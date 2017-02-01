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
  val x: Double = 0
  val y: Double = 0
  def children(): List[String]
  def name: String
  def attributes: List[Attribute]
  def setAttribute(attribute: Attribute)
  def deepClone(): NodeTrait[A]
  def deepClone(alternateChildren: List[String]): NodeTrait[A]
}

trait EdgeTrait[A] {
  val name: (String, String)
  val isDirected: Boolean
  val weight : Weight[A]
}

trait  GraphTrait[A] {
  val nodes: List[NodeTrait[A]]
  val edges: List[EdgeTrait[A]]
  def checkConsistency: Boolean
  def getEdge(n: NodeTrait[A]): List[EdgeTrait[A]]
  def getEdgeWithSrc(n: String): List[EdgeTrait[A]]
  def getEdgeWithDest(n: String): List[EdgeTrait[A]]
  def getNode(n: String): NodeTrait[A]
  def isDirected: Boolean

  def deepClone(): GraphTrait[A]
  def deepClone(n: List[NodeTrait[A]]): GraphTrait[A]
}

trait TreeTrait[A] extends GraphTrait[A] {
  def rootNode: NodeTrait[A]
}

trait OrderedTreeTrait[A <: Ordered[A]] extends TreeTrait[A]
trait OrderedGraphTrait[A <: Ordered[A]] extends GraphTrait[A]

trait NumericTreeTrait[A <: Numeric[A] with Ordered[A]] extends OrderedTreeTrait[A]
trait NumericGraphTrait[A <: Numeric[A] with Ordered[A]] extends OrderedGraphTrait[A]

trait GraphCreateFn[A] {
  def apply(g: GraphTrait[A]) : GraphTrait[A]
}








