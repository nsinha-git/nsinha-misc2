package com.nsinha.graph

import com.nsinha.graph.factories.{AssociativeNonDistributiveGraphFactory, GraphFactory, OrderedGraphFactory}
import com.nsinha.graph.interfaces.Common.{AssociativeNonDistributiveRingElem, OrderedOpaqueClass}
import com.nsinha.graph.interfaces.Ops.{AssociativeNonDistributiveGraphOps, OrderedGraphOps}
import com.nsinha.graph.interfaces._
import com.nsinha.graph.utils.dot.{DotReaderImpl, DotReaderImplForAssociativeNonDistributiveRingGraphs}
import org.scalatest.{FunSuite, MustMatchers}

/** Created by nsinha on 2/17/17.
  */
class TestRingGraphs extends FunSuite {

  test("just an associative non distributive graph") {
    val _g = AssociativeNonDistributiveGraphFactory.createRandomGraph(5, 1)
    val gOps = new AssociativeNonDistributiveGraphOps[AssociativeNonDistributiveRingElem] {
      override val g = _g
      override val Zero : AssociativeNonDistributiveRingElem = AssociativeNonDistributiveRingElem("0")
    }
    gOps.printGraphDot("/tmp/1")
  }

  test("find the all src shortest path") {
    for {
      i ← Range(5, 21)
      times ← Range(1, 4)
    } {

      val _g = AssociativeNonDistributiveGraphFactory.createRandomGraph(i, 1)
      val gOps = new AssociativeNonDistributiveGraphOps[AssociativeNonDistributiveRingElem] {
        override val g = _g
        override val Zero = AssociativeNonDistributiveRingElem("0")
      }

      val list = gOps.findMinDistForEachSrcDest
      list foreach { el ⇒
        // println(s"src=${el._1} dest=${el._2}  path= ${el._3} weight = ${el._4.name}")
      }
    }
  }

  test("a max flow problem") {
    val _g = AssociativeNonDistributiveGraphFactory.createRandomGraph(4, 1)
    val gOps = new AssociativeNonDistributiveGraphOps[AssociativeNonDistributiveRingElem] {
      override val g = _g
      override val Zero = AssociativeNonDistributiveRingElem("0")
    }
    gOps.printGraphDot("/tmp/1")
    val flowOpt = gOps.findMaxFlowSrcDest("n0", "n3")
    flowOpt match {
      case Some(flow) ⇒
        println(s"maxFlow = ${flow._1}")
        flow._2 foreach { x ⇒
          println(s"${x._1}")
          println(x._2.mkString("-"))
        }
      case None ⇒
        println("No flow")

    }
  }

  test("read a dot file") {
    val dotReader = new DotReaderImplForAssociativeNonDistributiveRingGraphs
    val _g = dotReader.readFileIntoGraph("/tmp/1")

    val gOps = new AssociativeNonDistributiveGraphOps[AssociativeNonDistributiveRingElem] {
      override val g = _g
      override val Zero = AssociativeNonDistributiveRingElem("0")
    }

    gOps.printGraphDot("/tmp/2")
  }

  test ("read a graph from clrs and check if the max flow works") {
    val dotReader = new DotReaderImplForAssociativeNonDistributiveRingGraphs
    val _g = dotReader.readFileIntoGraph("/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/test/resources/clrsFlowGraph.dot")

    val gOps = new AssociativeNonDistributiveGraphOps[AssociativeNonDistributiveRingElem] {
      override val g = _g
      override val Zero = AssociativeNonDistributiveRingElem("0")
    }

    gOps.printGraphDot("/tmp/1")
    val flowOpt = gOps.findMaxFlowSrcDest("Vancouver", "Winnipeg")
    flowOpt match {
      case Some(flow) ⇒
        println(s"maxFlow = ${flow._1}")
        flow._2 foreach { x ⇒
          println(s"${x._1}")
          println(x._2.mkString("-"))
        }
      case None ⇒
        println("No flow")
    }
  }
}
