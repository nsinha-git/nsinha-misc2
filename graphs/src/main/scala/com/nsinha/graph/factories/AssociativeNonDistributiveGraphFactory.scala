package com.nsinha.graph.factories

import com.nsinha.graph.factories.OrderedGraphFactory.getConnections
import com.nsinha.graph.interfaces.Common.{AssociativeNonDistributiveRingElem, OrderedOpaqueClass, RingElem, Weight}
import com.nsinha.graph.interfaces.Graph.{OrderedEdgeTrait, GraphOrdered, GraphTrait, Node}

import scala.util.control.NonFatal

/** Created by nsinha on 2/16/17.
  */
object AssociativeNonDistributiveGraphFactory {
  def createRandomGraph(n : Int, edgeProb : Double) : GraphTrait[AssociativeNonDistributiveRingElem] = {
    val totalNodes = n
    val directed : Boolean = true
    val nodes = for (i ← Range(0, totalNodes).toList) yield {
      val l = try {
        getConnections(i, totalNodes, directed, RandomGraph, edgeProb)
      }
      catch {
        case e : Exception if NonFatal(e) ⇒ List[Int]()
      }
      new Node(s"n$i", Math.cos(Math.PI * 2 * i / totalNodes), Math.sin(Math.PI * 2 * i / totalNodes), l map (x ⇒ s"n$x"))
    }

    new GraphOrdered[AssociativeNonDistributiveRingElem](_nodes = nodes, _isDirected = directed, (x : (String, String), y : Int) ⇒ new Weight[AssociativeNonDistributiveRingElem] {
      override val getWeight = new AssociativeNonDistributiveRingElem(math.random().toString)
    })
  }

  def createGraphFromEdgesAndNodes[A <: Ordered[A]](_nodesNames : List[String], _edges : List[OrderedEdgeTrait[A]], _isDirected : Boolean, generateWeight : ((String, String), Int) ⇒ Weight[A]) : GraphOrdered[A] = {
    GraphOrdered.createAGraph[A](_nodesNames, _edges, _isDirected, generateWeight)
  }

}
