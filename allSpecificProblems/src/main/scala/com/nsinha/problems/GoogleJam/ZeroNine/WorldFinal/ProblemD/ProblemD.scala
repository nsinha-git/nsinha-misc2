package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemD

import scala.collection.mutable
import java.lang.Integer._

import org.scalatest.FunSuite

/** Created by nsinha on 4/10/17.
  */

case class SpanNode(x : Int, y : Int)
case class ProblemD(inputs : String) {
  type SpanNodeName = String
  type Cost = Int
  val graph = mutable.HashMap[SpanNodeName, mutable.MutableList[String]]()
  val nameToSpanNodeMap = mutable.HashMap[SpanNodeName, SpanNode]()
  val costSpanNode = mutable.HashMap[SpanNodeName, Int]()
  val rangeSpanNode = mutable.HashMap[SpanNodeName, Int]()
  val aggregateSearchSpan = mutable.HashMap[SpanNodeName, mutable.Set[SpanNodeName]]()
  val ordering = new Ordering[(SpanNodeName, Cost)] {
    type T = (SpanNodeName, Cost)
    override def compare(x : T, y : T) = {
      val x1 = implicitly[Ordering[String]].compare(x._1, y._1)
      val x2 = implicitly[Ordering[Int]].compare(x._2, y._2)
      val res = if (x2 == 0) x1 else -x2
      res
    }
  }
  val sortedAggregateSearchSpan = mutable.TreeSet[(SpanNodeName, Cost)]()(ordering)
  val nodeToCost = mutable.HashMap[SpanNodeName, Cost]()
  var nodeName = 0

  processInputs
  calculateGraph
  createSearchSpanFromEveryNodeWitAggregateCost
  println(accumulatePositiveCosts)

  def createNodeName : SpanNodeName = {
    val ret = nodeName.toString
    nodeName = nodeName + 1
    ret
  }

  def processInputs = {
    val lines = inputs.split("\n")
    val numNodes = parseInt(lines.head)
    for (line ← lines.drop(1)) {
      val nodeName = createNodeName
      val allValues = line.split(" ")
      val xValue = parseInt(allValues(0))
      graph += nodeName → mutable.MutableList()
      allValues.drop(1) zip Range (0, 3) map {
        case (value, index) ⇒
          val intValue = Integer.parseInt(value)
          index match {
            case 0 ⇒
              nameToSpanNodeMap += nodeName → SpanNode(xValue, intValue)
            case 1 ⇒
              rangeSpanNode += nodeName → intValue
            case 2 ⇒
              costSpanNode += nodeName → intValue
          }
      }
    }
  }

  def distance(s1 : SpanNode, s2 : SpanNode) : Double = {
    val xdiff = s1.x - s2.x
    val ydiff = s1.y - s2.y
    val sum = xdiff * xdiff + ydiff * ydiff
    math.sqrt(sum)
  }

  def calculateGraph = {
    //this is n^2 op
    //create a distance matrix from each node to every node and then filter it for creating graph. ideally daoble
    //in o(n^2/2) with a more complex code which is here
    val nameToSpanNodeArray = nameToSpanNodeMap.toArray[(SpanNodeName, SpanNode)]

    Range(0, nameToSpanNodeArray.length) foreach { i ⇒
      Range (i + 1, nameToSpanNodeArray.length) foreach { j ⇒
        val (node1Name, node1Span) = nameToSpanNodeArray(i)
        val (node2Name, node2Span) = nameToSpanNodeArray(j)

        val dist = distance(node1Span, node2Span)
        val range1 = rangeSpanNode(node1Name)
        val range2 = rangeSpanNode(node2Name)
        if (dist < range1) graph(node1Name) += node2Name
        if (dist < range2) graph(node2Name) += node1Name
      }
    }
  }

  def accumulatePositiveCosts : Cost = {
    var cost = 0
    while (sortedAggregateSearchSpan.nonEmpty && sortedAggregateSearchSpan.head._2 > 0) {
      val head = sortedAggregateSearchSpan.head
      cost = cost + head._2
      aggregateSearchSpan(head._1) foreach (x ⇒ sortedAggregateSearchSpan.remove((x, nodeToCost(x))))
    }
    cost
  }

  def findNodesCost(s : mutable.Set[SpanNodeName]) : Cost = {
    var cost = 0
    s foreach (x ⇒ cost = costSpanNode(x) + cost)
    cost
  }

  def createSearchSpanFromEveryNodeWitAggregateCost = {
    graph.foreach {
      case (nodeName, nbrs) ⇒
        val mutableSetVisited = doDfs(nodeName, mutable.HashSet[String]())
        val findTotalCostOfNode = findNodesCost(mutableSetVisited)
        aggregateSearchSpan += nodeName → mutableSetVisited
        val costOfNode = findNodesCost(mutableSetVisited)
        nodeToCost += nodeName → costOfNode
        sortedAggregateSearchSpan += (nodeName → costOfNode)
    }
  }

  def doDfs(nodeName : SpanNodeName, visited : mutable.HashSet[String]) : mutable.HashSet[SpanNodeName] = {
    val nbrs = graph(nodeName) filter { x ⇒ !visited.contains(x) }
    visited += nodeName
    nbrs foreach (nbr ⇒ doDfs(nbr, visited))
    visited
  }

}

class Testing extends FunSuite {

  test ("a") {
    val inputs =
      """5
        |0 1 7 10
        |0 -1 7 10
        |5 0 1 -15
        |10 0 6 10
        |15 1 2 -20""".stripMargin
    ProblemD(inputs)

  }
}
