package com.nsinha.graph.utils.dot

import com.nsinha.graph.interfaces.Common.{AssociativeNonDistributiveRingElem, RingElem, Weight}
import com.nsinha.graph.interfaces.Graph.{Graph, GraphOrdered, GraphTrait, Node}
import com.nsinha.graph.interfaces._

import scala.collection.mutable
import scala.io.Source

/** Created by nsinha on 2/2/17.
  */
trait DotReaderToGraphTrait[A] {
  def readFileIntoGraph(fileName : String) : GraphTrait[A]
}

class DotReaderImplForAssociativeNonDistributiveRingGraphs extends DotReaderImpl[AssociativeNonDistributiveRingElem] with DotReaderToGraphTrait[AssociativeNonDistributiveRingElem] {

  val modifiedEdgeNode = "([a-zA-Z0-9]+)[ ]*->[ ]*([a-zA-Z0-9]+)\\[(.*)\\]".r

  val nodeToNodesWithWeight : mutable.Map[String, mutable.Set[(String, Double)]] = mutable.Map()

  override def readFileIntoGraph(fileName : String) : GraphTrait[AssociativeNonDistributiveRingElem] = {
    //a very procedural way of reading a graph. I intend to use a parser in long run.  I beseech you Dont judge me please or I may judge you too.
    //assuming very fixed formatting for now
    val src : Source = Source.fromFile(fileName)
    var ignore = false

    for (line ← src.getLines()) {
      if (ignore) {
      }
      else {
        //does it contain ->
        if (line.contains("{") || line.contains("}")) {} else if (line.contains("->")) {
          //an edge
          val modifiedEdgeNode(n1, n2, attrs) = line
          val weight = { attrs.split(";") filter (_.contains("label=")) }.head.split("=")(1).toDouble

          nodeToNodesWithWeight.get(n1) match {
            case None    ⇒ nodeToNodesWithWeight.+=((n1, mutable.Set((n2, weight))))
            case Some(x) ⇒ x += ((n2, weight))
          }
          nodeToNodesWithWeight.get(n2) match {
            case None ⇒ nodeToNodesWithWeight.+=((n2, mutable.Set()))
            case _    ⇒
          }
        }
        else if (line.size > 0) {
          // a node
          val regexNode(nodeName, attrs) = line
          nodeToNodesWithWeight.get(nodeName) match {
            case None ⇒ nodeToNodesWithWeight.+=((nodeName, mutable.Set()))
            case _    ⇒
          }
        }
      }
    }

    val nodes = nodeToNodesWithWeight zip Range(0, nodeToNodesWithWeight.size) map { x ⇒

      val kids : Set[(String, Double)] = x._1._2.toSet

      new Node(x._1._1, Math.cos(Math.PI * 2 * x._2 / nodeToNodesWithWeight.size), Math.sin(Math.PI * 2 * x._2 / nodeToNodesWithWeight.size), { kids map (_._1) }.toList, Nil)
    } toList

    val weightDictionary : Map[(String, String), AssociativeNonDistributiveRingElem] = {
      nodeToNodesWithWeight.foldLeft(Map[(String, String), AssociativeNonDistributiveRingElem]()) { (z, el) ⇒
        z ++ {
          el._2 map { y ⇒
            ((el._1, y._1), new AssociativeNonDistributiveRingElem(y._2.toString))
          } toMap
        }
      }
    }

    new GraphOrdered[AssociativeNonDistributiveRingElem](_nodes = nodes, _isDirected = true, (x : (String, String), y : Int) ⇒ new Weight[AssociativeNonDistributiveRingElem] {
      override val getWeight = weightDictionary(x)
    })
  }
}

class DotReaderImpl[A] extends DotReaderToGraphTrait[A] {
  val regexNode = "([a-zA-Z0-9]+).*\\[(.*)\\]".r
  val edgeNode = "([a-zA-Z0-9]+)[ ]*->[ ]*([a-zA-Z0-9]+)".r
  val nodeToNodes : mutable.Map[String, mutable.Set[String]] = mutable.Map()

  override def readFileIntoGraph(fileName : String) : GraphTrait[A] = {
    //a very procedural way of reading a graph. I intend to use a parser in long run.  I beseech you Dont judge me please or I may judge you too.
    //assuming very fixed formatting for now
    val src : Source = Source.fromFile(fileName)
    var ignore = false

    for (line ← src.getLines()) {
      if (ignore) {

      }
      else {
        //does it contain ->
        if (line.contains("{") || line.contains("}")) {} else if (line.contains("->")) {
          //a edge
          val edgeNode(n1, n2) = line
          nodeToNodes.get(n1) match {
            case None    ⇒ nodeToNodes.+=((n1, mutable.Set(n2)))
            case Some(x) ⇒ x.+=(n2)
          }
          nodeToNodes.get(n2) match {
            case None ⇒ nodeToNodes.+=((n2, mutable.Set()))
            case _    ⇒
          }
        }
        else if (line.size > 0) {
          // a node
          val regexNode(nodeName, attrs) = line
          nodeToNodes.get(nodeName) match {
            case None ⇒ nodeToNodes.+=((nodeName, mutable.Set()))
            case _    ⇒
          }
        }
      }
    }

    val nodes = nodeToNodes zip Range(0, nodeToNodes.size) map { x ⇒
      new Node(x._1._1, Math.cos(Math.PI * 2 * x._2 / nodeToNodes.size), Math.sin(Math.PI * 2 * x._2 / nodeToNodes.size), x._1._2.toList)
    } toList

    new Graph[A](_nodes = nodes, _isDirected = true, (x : (String, String), y : Int) ⇒ new Weight[A] {
      override val getWeight : A = ???
    })
  }
}
