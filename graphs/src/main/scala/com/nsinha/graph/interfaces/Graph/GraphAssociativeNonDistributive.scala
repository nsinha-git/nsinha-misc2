package com.nsinha.graph.interfaces.Graph

import com.nsinha.graph.interfaces.Common.{RingElem, Weight}

/** Created by nsinha on 2/16/17.
  */
class GraphAssociativeNonDistributive[A <: RingElem[A]](_nodes : List[NodeTrait], _isDirected : Boolean, generateWeight : ((String, String), Int) ⇒ Weight[A]) extends Graph[A](_nodes, _isDirected, generateWeight) with AssociativeNonDistributiveGraphTrait[A] {

  override protected def generateEdgesFromNodes(nodes : List[NodeTrait]) : List[OrderedEdgeTrait[A]] = {
    val x = (nodes zip Range(0, nodes.length)) flatMap {
      elem ⇒
        val subList : List[OrderedEdgeTrait[A]] = elem match {
          case (node, cnt) ⇒
            val src = node.name
            node.children() map {
              x ⇒
                new OrderedEdgeTrait[A] {
                  var name = (src, x)
                  var isDirected = _isDirected
                  var weight = generateWeight(name, cnt)
                  override def compare(that : OrderedEdgeTrait[A]) : Int = weight.getWeight.compare(that.weight.getWeight)
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
          List(el, new OrderedEdgeTrait[A] {
            var name = (el.name._2, el.name._1)
            var isDirected = _isDirected
            var weight = el.weight
            override def compare(that : OrderedEdgeTrait[A]) : Int = weight.getWeight.compare(that.weight.getWeight)
          })
      }
      z.toSet.toList
    }
    else y
  }
}
