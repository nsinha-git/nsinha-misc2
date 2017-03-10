package com.nsinha.problems.MaxFlow

import com.nsinha.common.{Node, UnDirectedGraph, UnDirectedGraphWithEdgeWeights}
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/9/17.
  */

case class MaxFlowNode(name : String) extends Node[MaxFlowNode] {
  override val children : mutable.MutableList[MaxFlowNode] = mutable.MutableList[MaxFlowNode]()
  override val parents = mutable.MutableList[MaxFlowNode]()
  var flow = mutable.Map[(String, String), (Int, Int)] ()
}

class MaxFlowEC(g : UnDirectedGraphWithEdgeWeights[MaxFlowNode], src : String, dest : String) {
  val srcNode = g.mapOfNamesToNodes(src)
  val destNode = g.mapOfNamesToNodes(dest)

  initFlow(g)

  def initFlow(_g : UnDirectedGraphWithEdgeWeights[MaxFlowNode]) = {
    _g.nodes map {
      x ⇒
        val src = x.name
        x.children map {
          child ⇒ x.flow += ((src, child.name) → (0, 0))
        }
    }
  }

  def findMaxFlow() : Int = {
    var flow = 0

    var cond = true
    while (cond) {
      val que = mutable.Queue[(MaxFlowNode, List[MaxFlowNode])]()
      que += ((srcNode, Nil))
      val visited = mutable.HashSet[MaxFlowNode]()
      visited += srcNode
      val path : List[MaxFlowNode] = doBfsInt(que, visited)
      if (path.size != 0) {
        val minFlow = findMinFlowOfPath(path)
        subtractFlow(path, minFlow)
        flow = flow + minFlow
      }
      else {
        cond = false
      }
    }
    flow
  }

  def findMinFlowOfPath(path : List[MaxFlowNode]) : Int = {
    { path zip path.slice(1, path.size) }.foldLeft(Integer.MAX_VALUE) {
      (Z, el) ⇒
        val weightOfEdge = el._1.weightMaps(el._2.name)
        val flowOFEdge = el._1.flow((el._1.name, el._2.name))._1
        val allowedFlow = weightOfEdge - flowOFEdge
        if (Z < allowedFlow) {
          Z
        }
        else {
          allowedFlow
        }
    }
  }

  def subtractFlow(path : List[MaxFlowNode], subtrand : Int) = {
    { path zip path.slice(1, path.size) } map {
      el ⇒
        val flowSrcOfEdgeInDir = el._1.flow((el._1.name, el._2.name))._1 + subtrand
        val flowSrcOfEdgeOtherDir = el._1.flow((el._1.name, el._2.name))._2 - subtrand

        el._1.flow.remove((el._1.name, el._2.name))
        el._1.flow += ((el._1.name, el._2.name)) → (flowSrcOfEdgeInDir, flowSrcOfEdgeOtherDir)

        val flowDestOfEdgeInDir = el._2.flow((el._2.name, el._1.name))._1 - subtrand
        val flowDestOfEdgeOtherDir = el._2.flow((el._2.name, el._1.name))._2 + subtrand

        el._2.flow.remove((el._2.name, el._1.name))
        el._2.flow += ((el._2.name, el._1.name)) → (flowDestOfEdgeInDir, flowDestOfEdgeOtherDir)
    }

  }

  def doBfsInt(que : mutable.Queue[(MaxFlowNode, List[MaxFlowNode])], visited : mutable.Set[MaxFlowNode]) : List[MaxFlowNode] = {
    if (que.isEmpty) {
      Nil
    }
    else {
      //start vsisting FIFO node from que
      val curVisit = que.dequeue()
      val curVisitNode = curVisit._1
      val curVisitNodePath = curVisit._2

      //check if this node is destNode and return prematurely
      if (curVisitNode == destNode) {
        return curVisitNodePath :+ curVisitNode
      }

      //enque the children
      for (childNode ← curVisitNode.children if !visited.contains(childNode)) yield {
        val weightForChildNode = curVisitNode.weightMaps(childNode.name)
        val flowForChildNode = curVisitNode.flow((curVisitNode.name, childNode.name))

        if (flowForChildNode._1 < weightForChildNode) {
          que += ((childNode, curVisitNodePath :+ curVisitNode))
          visited += childNode
        }
      }
      //finish visiting this node
      doBfsInt(que, visited)
    }
  }
}

class Testing extends FunSuite {

  def createNode(name : String) : MaxFlowNode = {
    MaxFlowNode(name)
  }

  test("a") {
    val nodes = List("n0", "n1", "n2", "n3")
    val edges = Map (
      "n0" → List(("n1", 1000), ("n2", 1000)),
      "n1" → List(("n2", 1), ("n3", 1000)),
      "n2" → List(("n3", 1000))
    )
    val graph = new UnDirectedGraphWithEdgeWeights[MaxFlowNode](nodes, edges)(createNode)

    val problem = new MaxFlowEC(graph, "n0", "n3")

    println(problem.findMaxFlow())

  }

  test("b") {
    val nodes = List("n0", "n1", "n2", "n3", "n4", "n5", "n6", "n7")
    val edges = Map (
      "n0" → List(("n1", 1), ("n2", 1), ("n3", 1)),
      "n1" → List(("n2", 1)), "n2" → List(("n3", 1)), "n3" → List(("n4", 1), ("n6", 1)),
      "n4" → List(("n5", 1), ("n7", 1)), "n6" → List(("n5", 1)),
      "n7" → List(("n5", 1))
    )
    val graph = new UnDirectedGraphWithEdgeWeights[MaxFlowNode](nodes, edges)(createNode)

    val problem = new MaxFlowEC(graph, "n0", "n3")

    println(problem.findMaxFlow())

  }

}
