package com.nsinha.graph.algorithms.Mst

import com.nsinha.graph.interfaces.{EdgeTrait, GraphOrdered, GraphTrait, NodeTrait}

import scala.collection.mutable

/**
  * Created by nsinha on 2/13/17.
  */
class PrimsMst[A <: Ordered[A]](g: GraphTrait[A]) {
  type PqueType = EdgeTrait[A]
  implicit val ord = new Ordering[PqueType] {
    override def compare(x : PqueType, y : PqueType): Int = {
      y.weight.getWeight.compare(x.weight.getWeight)
    }
  }
  val pQue = new mutable.PriorityQueue[PqueType]()

  def doMst: Option[GraphOrdered[A]] = {
    val processedQ = new mutable.Queue[String]()
    val edgesTaken = new mutable.MutableList[EdgeTrait[A]]()
    var ni = g.nodes.head
    var break = false
    while (!break)  {
      val allOutGoingEdgesNi = g.getEdgeWithSrc(ni.name) filter (x => !checkIfDestInQ(x, processedQ))
      val allIncomingEdgesNi = g.getEdgeWithDest(ni.name) filter (x => !checkIfSrcInQ(x, processedQ))
      processedQ.enqueue(ni.name)
      if(processedQ.size == g.nodes.size) break = true
      if(!break) {
        (allIncomingEdgesNi ++ allOutGoingEdgesNi) foreach(x => pQue.enqueue(x))
        val newEdge = pQue.dequeue()
        edgesTaken.+=(newEdge)
        val (src, dest) = newEdge.name
        //we need to remove all edges from pQue that have dest as their end. Check fib que ariation inn KruskalMSt
        //we will just satisfy  oursleves with a hack this time.
        removeDest(dest)
        ni = if (processedQ.contains(src)) g.getNode(dest) else g.getNode(src)
      }
    }
    Option(GraphOrdered.createAGraph[A](processedQ.toSet.toList, edgesTaken.toList, g.isDirected, g.getWeightFn))
  }


  def checkIfDestInQ(e: EdgeTrait[A] , q: mutable.Queue[String]):Boolean = {
    q.contains(e.name._2)
  }
  def checkIfSrcInQ(e: EdgeTrait[A] , q: mutable.Queue[String]):Boolean = {
    q.contains(e.name._1)
  }

  def removeDest(dest: String): Unit = {
    val dequed = pQue.dequeueAll
    dequed map {x =>
      if (x.name._2 != dest) {
      pQue.enqueue(x)
    } }
  }
}
