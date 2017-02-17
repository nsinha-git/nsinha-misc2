package com.nsinha.graph.interfaces.Graph

import com.nsinha.graph.interfaces.Common.{RingElem, Weight}

trait GraphTrait[A] {
  val nodes : List[NodeTrait]
  val edges : List[OrderedEdgeTrait[A]]
  def checkConsistency : Boolean
  def getEdge(n : NodeTrait) : List[OrderedEdgeTrait[A]]
  def getEdgeWithSrc(n : String) : List[OrderedEdgeTrait[A]]
  def getEdgeWithDest(n : String) : List[OrderedEdgeTrait[A]]
  def getNode(n : String) : NodeTrait
  def isDirected : Boolean
  def getWeightFn : ((String, String), Int) ⇒ Weight[A]

  def deepClone() : GraphTrait[A]
  def deepClone(n : List[NodeTrait]) : GraphTrait[A]
}

trait TreeTrait[A] extends GraphTrait[A] {
  def rootNode : NodeTrait
  def graph : GraphTrait[A]
}

trait OrderedTreeTrait[A <: Ordered[A]] extends TreeTrait[A]
trait OrderedGraphTrait[A <: Ordered[A]] extends GraphTrait[A]

trait AssociativeNonDistributiveGraphTrait[A <: RingElem[A]] extends GraphTrait[A]
trait AssociativeDistributiveGraphTrait[A <: RingElem[A]] extends GraphTrait[A]

trait NumericTreeTrait[A <: Numeric[A] with Ordered[A]] extends OrderedTreeTrait[A]
trait NumericGraphTrait[A <: Numeric[A] with Ordered[A]] extends OrderedGraphTrait[A]

trait GraphCreateFn[A] {
  def apply(g : GraphTrait[A]) : GraphTrait[A]
}

