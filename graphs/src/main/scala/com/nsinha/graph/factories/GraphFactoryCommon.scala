package com.nsinha.graph.factories

import com.nsinha.graph.appConfig.ApplicationConfig

import scala.io.StdIn
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

/** Created by nsinha on 2/8/17.
  */
class GraphFactoryCommon {
  def getConnections(curNode : Int, totalNodes : Int, directed : Boolean, style : InputStyle = input, prob : Double = randomGraphProb) : List[Int] = {
    val l : List[Int] = style match {
      case UserInteraction ⇒
        try {
          readFromStdin[String](StdIn.readLine).head split ("[ ,]") map (_.toInt) filter (x ⇒ if (x <= totalNodes && x != curNode) true else false) toList
        }
        catch {
          case e : Exception if NonFatal(e) ⇒ List[Int]()
        }

      case CompleteGraph ⇒
        Range(0, totalNodes).toList.map(_.toInt) filter (x ⇒ x != curNode)

      case RandomGraph ⇒
        Range(0, totalNodes).toList.map(_.toInt) filter (x ⇒ x != curNode) filter (x ⇒ if (Random.nextDouble() < prob) true else false)

    }
    l
  }

  def readFromStdin[A](fn : ⇒ A) : List[A] = {
    var l = List[A]()
    try {
      val x = fn
      l = l.+:(x)
      l
    }
    catch {
      case e : Exception ⇒
    }
    l
  }

  val input : InputStyle = {
    val inputStyle = Try({
      ApplicationConfig.conf.getString("graph.connections")
    })
    inputStyle match {
      case Success("random")      ⇒ RandomGraph
      case Success("complete")    ⇒ CompleteGraph
      case Success("interactive") ⇒ UserInteraction
      case Success(x) ⇒
        println(s"Fix the graph.connection. Unknown vale $x")
        RandomGraph
      case Failure(ex) ⇒
        println("Fix the graph.connection. Failure reading value ")
        RandomGraph
    }
  }

  val randomGraphProb : Double = {
    val x = Try {
      ApplicationConfig.conf.getDouble("graph.randomConnectionsProb")
    }
    x match {
      case Success(x)  ⇒ x
      case Failure(ex) ⇒ 0.5
    }
  }
}
