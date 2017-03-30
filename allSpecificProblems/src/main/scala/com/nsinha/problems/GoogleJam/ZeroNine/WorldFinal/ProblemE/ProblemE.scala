package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemE

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/29/17.
  */

case class SpanNode(left : Int, right : Int)
case class ProblemE(input : String) {
  type Pos = Int

  val allSpanNode : List[SpanNode] = getSpanNodes

  val spanNodeGraph : Map[SpanNode, List[SpanNode]] = createEdges
  val dirsAvailable = Set("up", "down")

  println({ HeightForGraph(spanNodeGraph, allSpanNode) }.solve)

  def getSpanNodes : List[SpanNode] = {
    val colors = input.split(" ")
    val colMap = mutable.Map[String, mutable.MutableList[Pos]] ()

    colors zip Range(0, colors.size) foreach { col ⇒
      if (!colMap.contains(col._1)) colMap += col._1 → mutable.MutableList[Pos]()
      colMap(col._1) += col._2
    }

    colMap map { x ⇒
      val a = x._2(0)
      val b = x._2(1)
      val left = if (a > b) b else a
      val right = if (a > b) a else b
      SpanNode(left, right)
    } toList
  }

  def createEdges : Map[SpanNode, List[SpanNode]] = {
    val mapOfNodes = mutable.HashMap[SpanNode, mutable.Set[SpanNode]]()
    //do o(n^2)
    allSpanNode foreach {
      node ⇒
        mapOfNodes += node → mutable.HashSet[SpanNode]()
    }

    for (curNode ← allSpanNode) {
      for (potNeighborNode ← allSpanNode if (potNeighborNode != curNode)) {
        val inter = isInterSection(curNode, potNeighborNode)
        if (inter) {
          mapOfNodes(curNode) += potNeighborNode
          mapOfNodes(potNeighborNode) += curNode
        }
        else {
        }
      }
    }
    mapOfNodes map { x ⇒ x._1 → x._2.toList } toMap
  }

  def isInterSection(node1 : SpanNode, node2 : SpanNode) : Boolean = {
    if (node2.left < node1.right & node2.left > node1.left) return true
    if (node1.left < node2.right & node1.left > node2.left) return true
    false
  }
}

case class HeightForGraph(g : Map[SpanNode, List[SpanNode]], allNodes : List[SpanNode]) {
  val allNodesSet = allNodes.toSet

  def solve : Option[Int] = {
    if (g.size == 0) return Option(0)
    val allIps = findIndependentSets
    var max = 0
    var failure = false

    allIps foreach {
      thisIp ⇒
        if (!failure) {
          val (residulaNodes : List[SpanNode], maximalNodeOpt, residualGraph) = removeMaximal(thisIp)
          maximalNodeOpt match {
            case None ⇒
              failure = true

            case Some(maximalNode) ⇒
              val thisIpHtOpt = {
                HeightForGraph(residualGraph, residulaNodes)
              }.solve
              thisIpHtOpt match {
                case None           ⇒ failure = true
                case Some(thisIpHt) ⇒ if (max < (thisIpHt + 1)) { max = thisIpHt + 1 }
              }
          }
        }
    }

    if (!failure) Option(max) else None
  }

  def removeMaximal(ipList : List[SpanNode]) : (List[SpanNode], Option[SpanNode], Map[SpanNode, List[SpanNode]]) = {
    val (nodeOpt, gCur) = getMaximal(ipList)
    nodeOpt match {
      case None ⇒ (Nil, None, gCur)
      case Some(node) ⇒ (ipList filter (x ⇒ x != node), nodeOpt, {
        gCur filter (x ⇒ x._1 != node) map { x ⇒
          x._1 → x._2.filter { y ⇒ y != node }
        }
      }.filter(x ⇒ x._2.size != 0))
    }
  }

  def getMaximal(ip : List[SpanNode]) : (Option[SpanNode], Map[SpanNode, List[SpanNode]]) = {
    val ipSet = ip.toSet
    val sizeToCheck = ip.size - 1
    val gCur = g filter (x ⇒ ipSet.contains(x._1)) map { x ⇒ x._1 → x._2.filter (y ⇒ ipSet.contains(y)) }

    val maximals = mutable.MutableList[SpanNode]()

    ip foreach {
      x ⇒
        if (gCur(x).size == sizeToCheck) {
          maximals += x
        }
    }

    if (maximals.isEmpty) {
      (None, gCur)
    }
    else if (maximals.size > 1) {
      (maximals.sortBy(x ⇒ x.left - x.right).headOption, gCur)
    }
    else {
      (maximals.headOption, gCur)
    }
  }

  def findIndependentSets : List[List[SpanNode]] = {
    val visited = mutable.HashSet[SpanNode]()
    val unvisited = mutable.HashSet[SpanNode]()
    val ipSets = mutable.MutableList[List[SpanNode]]()

    allNodes foreach (node ⇒ unvisited += node)

    while (unvisited.nonEmpty) {
      val curNodeToVisit = unvisited.head
      val l : List[SpanNode] = doSearch(curNodeToVisit)
      l foreach { x ⇒
        unvisited.remove(x)
        visited.add(x)
      }
      ipSets += l
    }

    ipSets.toList
  }

  def doSearch(curNode : SpanNode) : List[SpanNode] = {
    val visited = mutable.HashSet[SpanNode]()
    doDfs(visited, curNode)
    visited.toList
  }

  def doDfs(visited : mutable.HashSet[SpanNode], curNode : SpanNode) : Unit = {
    visited += curNode

    val nbrs = g(curNode).toSet.intersect(allNodesSet)

    nbrs foreach (nbr ⇒ if (!visited.contains(nbr)) doDfs(visited, nbr))
  }

}

class Testing extends FunSuite {
  test("a") {
    ProblemE("red red blue yellow yellow blue")
  }
}

