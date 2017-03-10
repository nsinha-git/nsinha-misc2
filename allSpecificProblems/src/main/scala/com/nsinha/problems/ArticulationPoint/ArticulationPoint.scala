package com.nsinha.problems.ArticulationPoint

import com.nsinha.common.{Node, UnDirectedGraph}
import com.nsinha.problems.TwoDimMaxSum.Coordinate
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/7/17.
  */
case class TimeStampedNode(name : String) extends Node[TimeStampedNode] {
  override val children : mutable.MutableList[TimeStampedNode] = mutable.MutableList[TimeStampedNode] ()
  override val parents : mutable.MutableList[TimeStampedNode] = mutable.MutableList[TimeStampedNode] ()
  var t1 : Int = 0
  var t2 : Int = 0

}

class ArticulationPoint(g : UnDirectedGraph[TimeStampedNode]) {
  var ticks = -1

  def findAP() : Option[String] = {
    val dfsRoot = g.nodes.head
    ticks = -1
    doDfs(dfsRoot)
    recognizeAp(dfsRoot) map (_.name)
  }

  def doDfs(tsnode : TimeStampedNode, visited : mutable.HashSet[TimeStampedNode] = mutable.HashSet.empty[TimeStampedNode]) : Unit = {
    ticks = ticks + 1
    tsnode.t1 = ticks
    visited += tsnode
    for (x ← tsnode.children) yield {
      if (!visited.contains(x)) doDfs(x, visited)
    }
    tsnode.t2 = ticks
  }

  def recognizeAp(tsnode : TimeStampedNode) : Option[TimeStampedNode] = {
    val span = Coordinate(tsnode.t1, tsnode.t2)
    val kidSpans = for (x ← tsnode.children) yield { Coordinate(x.t1, x.t2) }
    var foundIntersection = true
    for {
      x ← kidSpans
      y ← kidSpans
    } {
      if (x.x == y.x && x.y == y.y) {

      }
      else {
        if (!isContained(x, y)) foundIntersection = false
      }
    }

    if (!foundIntersection) Option(tsnode) else {
      tsnode.children.foldLeft(Option[TimeStampedNode](null)) { (Z, el) ⇒
        Z match {
          case None    ⇒ recognizeAp(el)
          case Some(x) ⇒ Z
        }

      }
    }
  }

  def isContained(x : Coordinate, y : Coordinate) : Boolean = {
    isContainedLeft(x, y) || isContainedRight(x, y)
  }

  def isContainedLeft(x : Coordinate, y : Coordinate) : Boolean = {
    if (y.x < x.y && y.x > x.x) true else false
  }

  def isContainedRight(x : Coordinate, y : Coordinate) : Boolean = {
    isContainedLeft(y, x)
  }
}

class Testing extends FunSuite {

  def createNode(name : String) : TimeStampedNode = {
    TimeStampedNode(name)
  }

  test ("a") {
    val nodes = List("n0", "n1", "n2", "n3", "n4")
    val edges = Map (
      "n0" → List("n1", "n2"),
      "n1" → List("n3", "n4")
    )
    val graph = new UnDirectedGraph[TimeStampedNode](nodes, edges)(createNode)
    val ap = { new ArticulationPoint(graph) }
    println(ap.findAP())
  }

  test ("b") {
    val nodes = List("n0", "n1", "n2", "n3", "n4")
    val edges = Map (
      "n0" → List("n1", "n2"),
      "n1" → List("n3", "n4"), "n3" → List("n0")
    )
    val graph = new UnDirectedGraph[TimeStampedNode](nodes, edges)(createNode)
    val ap = { new ArticulationPoint(graph) }
    println(ap.findAP())
  }

}
