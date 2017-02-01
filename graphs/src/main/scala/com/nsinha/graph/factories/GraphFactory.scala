package com.nsinha.graph.factories

import com.nsinha.graph.interfaces._
import com.nsinha.graph
import com.nsinha.graph.appConfig.ApplicationConfig

import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}
import scala.util.control.NonFatal


/**
  * Created by nsinha on 1/27/17.
  */

class InputStyle
object UserInteraction extends InputStyle
object CompleteGraph extends InputStyle
object RandomGraph extends InputStyle

object GraphFactory {
  val input: InputStyle = {
    val inputStyle = Try({
      ApplicationConfig.conf.getString("graph.connections")
    })
    inputStyle match {
      case Success("random") => RandomGraph
      case Success("complete") => CompleteGraph
      case Success("interactive") => UserInteraction
      case Success(x) => println(s"Fix the graph.connection. Unknown vale $x")
          RandomGraph
      case Failure(ex) => println("Fix the graph.connection. Failure reading value ")
        RandomGraph
    }
  }

  val randomGraphProb: Double = {
    val x = Try {
      ApplicationConfig.conf.getDouble("graph.randomConnectionsProb")
    }
    x match {
      case Success(x) => x
      case Failure(ex) => 0.5
    }
  }




  def readFromStdin[A](fn: =>A): List[A] = {
    var l = List[A]()

    try {
      val x = fn
      l = l .+:(x)
      l
    } catch {
      case e : Exception =>
    }
    l
  }

  def getConnections(curNode: Int, totalNodes: Int, directed: Boolean): List[Int] = {
    val l : List[Int] = input match {
      case UserInteraction =>
        try {
          readFromStdin[String](StdIn.readLine).head split ("[ ,]") map (_.toInt) filter (x => if (x <= totalNodes && x != curNode) true else false) toList
        } catch {
          case e: Exception if NonFatal(e) => List[Int]()
        }

      case CompleteGraph =>
        Range(0, totalNodes).toList.map(_.toInt) filter (x => x != curNode)

      case RandomGraph =>
        Range(0, totalNodes).toList.map(_.toInt) filter (x => x != curNode) filter(x => if (Random.nextDouble() < randomGraphProb) true else false)


    }
    l
  }


  def createGraphOfOpaquesInteractive(): GraphTrait[OpaqeClass] = {
    println("We are interactively creating a graph of opaque objects")

    println("Enter max no of nodes")

    val totalNodes = readFromStdin[Int](StdIn.readInt()).head


    println("Enter if graph is directed: 1 for true or 0 for false")

    val directed: Boolean = readFromStdin[Int](StdIn.readInt()) map (x => x match {
      case 0 => false
      case 1 => true
    }) head


    val nodes = for (i <- Range(0, totalNodes).toList) yield {
      println(s"We are creating node n$i. \n Step 1. Enter the connectivity nodes.")
      val l = try {
        getConnections(i, totalNodes, directed)
      } catch {
        case e: Exception if NonFatal(e) => List[Int]()
      }
      println(s"Step 2.We are skipping the weight step for edges . We will create opaque weights for your covenience")

      new Node[OpaqeClass](s"n$i",Math.cos(Math.PI*2*i/totalNodes), Math.sin(Math.PI*2*i/totalNodes), l map (x => s"n$x"))
    }


    new Graph[OpaqeClass](_nodes = nodes, _isDirected = directed, (x: (String, String), y: Int) => new Weight[OpaqeClass] {
      override def getWeight = new OpaqeClass("opaqueWeight")
    })

  }

  //def createGraphOfStrings(n: List[NodeTrait[String]]): OrderedGraph[String] = ???
  //def createGraphOfDouble(n: List[NodeTrait[Double]]): NumericGraph[Double] = ???

}
