package com.nsinha.graph

import com.nsinha.graph.appConfig.ApplicationConfig
import com.nsinha.graph.factories.GraphFactory
import com.nsinha.graph.interfaces.Common.OpaqueClass
import com.nsinha.graph.interfaces.Ops.{GraphOpsTrait, TreeOpsTrait}
import com.nsinha.graph.utils.dot.DotReaderImpl
import org.scalatest.{FunSuite, MustMatchers}

/** Created by nsinha on 2/4/17.
  */
class TestGraph extends FunSuite with MustMatchers {
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
    val g1T = new GraphOpsTrait[OpaqueClass] {
      override val g : G = g1.transpose
    }
    g1.printGraphDot("/tmp/graph.dot")
    g1T.printGraphDot("/tmp/graphT.dot")

  }

  test("test fcc on random graph") {

    val g1 = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile1.dot")
    g1.printGraphDot()

    val sccs = g1.getFullyConnectedComponents
    sccs map { scc ⇒
      println(scc.name)
    }
  }

  test("test fcc on complete graph") {

    val g1 = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile2_fc.dot")
    g1.printGraph

    val sccs = g1.getFullyConnectedComponents
    sccs map { scc ⇒
      println(scc.name)
    }
  }

  test("performance test fcc on random graph") {
    for {
      i ← Range(5, 21)
      prob ← Range(1, 10)
      times ← Range(1, 4)
    } {
      val _g = GraphFactory.createGraphOfOpaquesRandom(i, prob / 10.0)
      val gOps = new GraphOpsTrait[OpaqueClass] { override val g = _g }
      gOps.getFullyConnectedComponents

    }

  }

  test ("dfs and tree traversal list") {
    val _g = GraphFactory.createGraphOfOpaquesRandom(6, .5)

    val gOps = new GraphOpsTrait[OpaqueClass] { override val g = _g }

    val treeDfsOpt = gOps.dfsTree("n0")
    treeDfsOpt map { _tree ⇒
      val treeOps = new TreeOpsTrait[OpaqueClass] {
        override val tree = _tree
        override val g = _tree.graph
      }
      treeOps.createAPreOrderedList map (x ⇒ print(x.name))
      println()
      treeOps.createAPostOrderedList map (x ⇒ print(x.name))
      treeOps.printGraphDot()
    }
  }

  test("test scc on random graph") {
    for {
      i ← Range(5, 21)
      prob ← Range(1, 10)
      times ← Range(1, 4)
    } {
      val _g = GraphFactory.createGraphOfOpaquesRandom(i, prob / 10.0)
      val gOps = new GraphOpsTrait[OpaqueClass] { override val g = _g }
      gOps.printGraphDot(s"/tmp/gfile${i}${prob}${times}")
      val scc = gOps.getStronglyConnectedComponents()
      scc map { sccelem ⇒
        println(sccelem.name)
      }
      println("\n\n")
    }
  }

  test("test bipartite on test graph") {

    {
      val gOps = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/dotfile1.dot")
      gOps.printGraphDot()
      val (l1, l2) = gOps.bipartite()
      l1 map (x ⇒ print(x))
      println("")
      l2 map (x ⇒ print(x))
      println("\n\n")
    }
    {
      val gOps = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/testGraphBipartite.dot")
      gOps.printGraphDot()
      val (l1, l2) = gOps.bipartite()
      l1 map (x ⇒ print(x))
      println("")
      l2 map (x ⇒ print(x))
      println("\n\n")
    }
  }

  test("test bipartite on random graph") {
    for {
      i ← Range(5, 21)
      prob ← Range(1, 10)
      times ← Range(1, 4)
    } {
      val _g = GraphFactory.createGraphOfOpaquesRandom(i, prob / 10.0)
      val gOps = new GraphOpsTrait[OpaqueClass] { override val g = _g }
      gOps.printGraphDot(s"/tmp/gfile${i}${prob}${times}")
      val (l1, l2) = gOps.bipartite()

      l1 map (x ⇒ print(x))
      println("")
      l2 map (x ⇒ print(x))
      println("/n/n")

    }
  }

  test("test topo ordering on random graph") {

    val gOps = DotReader("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/testGraphBipartite.dot")
    gOps.printGraphDot()

    gOps.topologicalSort() map {
      print(_)
    }
    println("\n\n")
  }

  test("test topo ordering on many random graph") {
    for {
      i ← Range(5, 21)
      prob ← Range(1, 10)
      times ← Range(1, 4)
    } {
      val _g = GraphFactory.createGraphOfOpaquesRandom(i, prob / 10.0)
      val gOps = new GraphOpsTrait[OpaqueClass] {
        override val g = _g
      }
      gOps.printGraphDot()

      gOps.topologicalSort() map {
        print(_)
      }
      println("\n\n")
      println("\n\n")

    }
  }

  test("test for cycles on many random graph") {
    for {
      i ← Range(5, 21)
      prob ← Range(1, 10)
      times ← Range(1, 4)
    } {
      val _g = GraphFactory.createGraphOfOpaquesRandom(i, prob / 10.0)
      val gOps = new GraphOpsTrait[OpaqueClass] {
        override val g = _g
      }
      gOps.printGraphDot()

      println(gOps.doesHaveCycles())
      println("\n\n")
      println("\n\n")

    }
  }

  def testDfs[A](g : GraphOpsTrait[A]) = {
    val tree = g.dfsTree("n0")

    tree.toList.map (x ⇒ {
      val gOps = new GraphOpsTrait[A] {

        override val g = x
      }
      gOps.printGraph
      gOps.printGraphDot("/tmp/testDfs")
    })
  }

  def testBfs[A](g : GraphOpsTrait[A]) = {
    val tree = g.bfsTree("n0")

    tree.toList.map (x ⇒ {
      val gOps = new GraphOpsTrait[A] {
        override val g = x
      }
      gOps.printGraph
      gOps.printGraphDot("/tmp/testBfs")
    })
  }

  def DotReader(fileName : String) = {
    val dotReader = new DotReaderImpl[OpaqueClass]
    val _g = dotReader.readFileIntoGraph(fileName)
    val graphOps = new GraphOpsTrait[OpaqueClass] {
      override val g : G = _g
    }
    graphOps
  }
}
