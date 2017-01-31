package com.nsinha.graph.application

import com.nsinha.graph.config.ApplicationConfig
import com.nsinha.graph.factories.GraphFactory
import com.nsinha.graph.interfaces.Graph

/**
  * Created by nsinha on 1/28/17.
  */
object TestOpaqueGraph {
  {
    ApplicationConfig
  }

  def main(args: Array[String]): Unit = {

    val g = GraphFactory.createGraphOfOpaquesInteractive()

    println(s"No of nodes= ${g.nodes.length}")
    println(s"No of edges= ${g.edges.length}")

    println("Nodes:")
    g.nodes map {x =>  print(s"${x.name}")}
    println()
    println("Edges:")
    g.edges map {e => print(s"${e.name}")}

  }



}
