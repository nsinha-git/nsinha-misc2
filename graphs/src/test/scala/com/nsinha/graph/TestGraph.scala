package com.nsinha.graph

import com.nsinha.graph.appConfig.ApplicationConfig
import com.nsinha.graph.interfaces.{GraphOpsTrait, OpaqeClass}
import com.nsinha.graph.utils.dot.DotReaderImpl
import org.scalatest.{FunSuite, MustMatchers}

/**
  * Created by nsinha on 2/4/17.
  */
class TestGraph extends FunSuite with MustMatchers{
  ApplicationConfig

  test("test dot reader") {
    val g = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile1.dot")

    g.printGraphDot()
  }

  test("test fully connected") {
    val g1 = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile1.dot")
    g1.isFullyConnected mustBe false

    val g2 = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile2_fc.dot")
    g2.isFullyConnected mustBe true
  }

  test("test transpose") {
    val g1 = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile2_fc.dot")
    val g1T = new GraphOpsTrait[OpaqeClass] {
      override val g: G = g1.transpose
    }
    g1.printGraphDot("/tmp/graph.dot")
    g1T.printGraphDot("/tmp/graphT.dot")

  }

  test("test scc on random graph") {

    val g1 = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile1.dot")
    g1.printGraphDot()

    val sccs = g1.getStronglyConnectedComponents
    sccs map {scc =>
      println(scc.name)
    }
  }

  test("test scc on complete graph") {

    val g1 = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile2_fc.dot")
    g1.printGraph

    val sccs = g1.getStronglyConnectedComponents
    sccs map {scc =>
      println(scc.name)
    }
  }


  def testDfs[A](g: GraphOpsTrait[A]) = {
    val tree = g.dfsTree("n0")

    tree.toList.map (x => {
      val gOps = new GraphOpsTrait[A] {
        override val g = x
      }
      gOps.printGraph
      gOps.printGraphDot("/tmp/testDfs")
    })
  }

  def testBfs[A](g: GraphOpsTrait[A]) = {
    val tree = g.bfsTree("n0")

    tree.toList.map (x => {
      val gOps = new GraphOpsTrait[A] {
        override val g = x
      }
      gOps.printGraph
      gOps.printGraphDot("/tmp/testBfs")
    })
  }

  def DotReader(fileName: String) = {
    val dotReader = new DotReaderImpl[OpaqeClass]
    val _g = dotReader.readFileIntoGraph(fileName)
    val graphOps = new GraphOpsTrait[OpaqeClass] {
      override val g: G = _g
    }
    graphOps
  }
}
