package com.nsinha.graph.algorithms.Mst

import com.nsinha.graph.algorithms.CommonAncestor.CommonAncestor
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

    implicit val ord = new Ordering[(String, EdgeTrait[A], Set[EdgeTrait[A]])] {
      override def compare(x : (String, EdgeTrait[A], Set[EdgeTrait[A]]), y : (String, EdgeTrait[A], Set[EdgeTrait[A]])) : Int = {
        if (x._2.weight.getWeight == y._2.weight.getWeight) {
          y._1.compare(x._1)
        }
        else {
          y._2.weight.getWeight.compare(x._2.weight.getWeight)
        }
      }

    }
    val pQue = new mutable.PriorityQueue[(String, EdgeTrait[A], Set[EdgeTrait[A]])]()

    //o(n)
    incomingSetsWithMin foreach { x ⇒
      pQue.enqueue((x._1, x._2._2, x._2._1))
    }

    //we need to repeat till the que is empty
    var listNodes : mutable.MutableList[String] = mutable.MutableList()
    var edges : mutable.MutableList[EdgeTrait[A]] = mutable.MutableList()
    val commonAncestor = new CommonAncestor
    while (pQue.nonEmpty) {
      val x = pQue.dequeue
      if (commonAncestor.checkCycleOK(x._2.name._1, x._2.name._2)) {
        listNodes += (x._2.name._1, x._2.name._2)
        edges += (x._2)
      }
      else {
        val set = x._3 diff (Set(x._2))
        val key = x._1
        if (set.nonEmpty) pQue.enqueue((key, set.min, set))
      }
    }

    //total is just o(E +nlogn). This beats cormen -rivest bound if this works.
    Option(GraphOrdered.createAGraph[A](listNodes.toSet.toList, edges.toList, g.isDirected, g.getWeightFn))
  }

}
