package com.nsinha.graph.factories

import com.nsinha.graph.interfaces._

import scala.util.control.NonFatal

/** Created by nsinha on 2/8/17.
  */
object OrderedGraphFactory extends GraphFactoryCommon {
  def createGraphOfOpaquesRandom(n : Int, edgeProb : Double) : GraphTrait[OrderedOpaqueClass] = {
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

    new Graph[OrderedOpaqueClass](_nodes = nodes, _isDirected = directed, (x : (String, String), y : Int) ⇒ new Weight[OrderedOpaqueClass] {
      override def getWeight = new OrderedOpaqueClass(math.random().toString)
    })
  }

}
