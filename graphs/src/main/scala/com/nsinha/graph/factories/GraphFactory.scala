package com.nsinha.graph.factories

import com.nsinha.graph.appConfig.ApplicationConfig
import com.nsinha.graph.interfaces._

import scala.io.StdIn
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

/** Created by nsinha on 1/27/17.
  */

class InputStyle
object UserInteraction extends InputStyle
object CompleteGraph extends InputStyle
object RandomGraph extends InputStyle

object GraphFactory extends GraphFactoryCommon {

  def createGraphOfOpaquesInteractive() : GraphTrait[OpaqueClass] = {
    println("We are interactively creating a graph of opaque objects")

    println("Enter max no of nodes")

    val totalNodes = readFromStdin[Int](StdIn.readInt()).head

    println("Enter if graph is directed: 1 for true or 0 for false")

    val directed : Boolean = readFromStdin[Int](StdIn.readInt()) map (x ⇒ x match {
      case 0 ⇒ false
      case 1 ⇒ true
    }) head

    val nodes = for (i ← Range(0, totalNodes).toList) yield {
      println(s"We are creating node n$i. \n Step 1. Enter the connectivity nodes.")
      val l = try {
        getConnections(i, totalNodes, directed)
      }
      catch {
        case e : Exception if NonFatal(e) ⇒ List[Int]()
      }
      println(s"Step 2.We are skipping the weight step for edges . We will create opaque weights for your covenience")

      new Node(s"n$i", Math.cos(Math.PI * 2 * i / totalNodes), Math.sin(Math.PI * 2 * i / totalNodes), l map (x ⇒ s"n$x"))
    }

    new Graph[OpaqueClass](_nodes = nodes, _isDirected = directed, (x : (String, String), y : Int) ⇒ new Weight[OpaqueClass] {
      override def getWeight = new OpaqueClass("opaqueWeight")
    })
  }

  def createGraphOfOpaques[A](nodes : List[NodeTrait], directed : Boolean = true, generateWeightFn : ((String, String), Int) ⇒ Weight[A]) : GraphTrait[A] = {
    new Graph[A](_nodes = nodes, _isDirected = directed, generateWeightFn)
  }

  def createGraphOfOpaquesRandom(n : Int, edgeProb : Double) : GraphTrait[OpaqueClass] = {
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

    new Graph[OpaqueClass](_nodes = nodes, _isDirected = directed, (x : (String, String), y : Int) ⇒ new Weight[OpaqueClass] {
      override def getWeight = new OpaqueClass("opaqueWeight")
    })
  }

  //def createGraphOfStrings(n: List[NodeTrait[String]]): OrderedGraph[String] = ???
  //def createGraphOfDouble(n: List[NodeTrait[Double]]): NumericGraph[Double] = ???
}
