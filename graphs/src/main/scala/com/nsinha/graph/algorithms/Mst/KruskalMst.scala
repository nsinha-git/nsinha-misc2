package com.nsinha.graph.algorithms.Mst

import com.nsinha.graph.interfaces._

import scala.collection.mutable

/** Created by nsinha on 2/9/17.
  */

//sort by edges
class KruskalMst[A <: Ordered[A]](g : GraphTrait[A]) {

  def createSetsOfIncomingEdgesEquivalenceClassGroupedByDest() : Map[String, Set[EdgeTrait[A]]] = {
    val mp = mutable.Map[String, List[EdgeTrait[A]]]()
    //o(E)
    g.nodes foreach { el ⇒ mp.+=(el.name → g.getEdgeWithDest(el.name)) }
    mp map (x ⇒ x._1 → x._2.toSet) toMap
  }

  def doMst : Option[GraphOrdered[A]] = {
    //O(E)
    val incomingSetsWithMin = createSetsOfIncomingEdgesEquivalenceClassGroupedByDest() map { x ⇒ x._1 → (x._2, x._2.min) }

    implicit val ord = new Ordering[(String, EdgeTrait[A])] {
      override def compare(x : (String, EdgeTrait[A]), y : (String, EdgeTrait[A])) : Int = {
        if (x._2.weight.getWeight == y._2.weight.getWeight) {
          y._1.compare(x._1)
        }
        else {
          y._2.weight.getWeight.compare(x._2.weight.getWeight)
        }
      }

    }
    val pQue = new mutable.PriorityQueue[(String, EdgeTrait[A])]()

    //o(n)
    incomingSetsWithMin foreach { x ⇒ pQue.enqueue((x._1 → x._2._2)) }

    //we need to repeat till the que is empty
    var listNodes : mutable.MutableList[String] = mutable.MutableList()
    var edges : mutable.MutableList[EdgeTrait[A]] = mutable.MutableList()
    var i = 0
    while (pQue.nonEmpty & i < (g.nodes.size - 1)) { //every loop costs o(logn) . every node adds one loop, so nlogn
      val x = pQue.dequeue()
      listNodes += (x._2.name._1, x._2.name._2)
      edges += (x._2)
      i = i + 1
    }

    //total is just o(E +nlogn). This beats cormen -rivest bound if this works.
    Option(GraphOrdered.createAGraph[A](listNodes.toSet.toList, edges.toList, g.isDirected, g.getWeightFn))
  }

}
