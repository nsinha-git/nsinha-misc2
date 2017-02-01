package com.nsinha.graph.application

import com.nsinha.graph.appConfig.ApplicationConfig
import com.nsinha.graph.factories.GraphFactory
import com.nsinha.graph.interfaces.{Graph, GraphOpsTrait, OpaqeClass}

/**
  * Created by nsinha on 1/28/17.
  */
object TestOpaqueGraph {
  {
    ApplicationConfig
  }

  def main(args: Array[String]): Unit = {
    val g = GraphFactory.createGraphOfOpaquesInteractive()
    val graphOps = new GraphOpsTrait[OpaqeClass] {}

    graphOps.printGraph(g)
    //graphOps.createSpaceEmbeddedGnuplotData(g)
    val tree = graphOps.bfsTree("n0", g)
    tree.toList.map (x => graphOps.printGraph(x) )
    tree map (x =>graphOps.createSpaceEmbeddedGnuplotData(x))
  }



}
