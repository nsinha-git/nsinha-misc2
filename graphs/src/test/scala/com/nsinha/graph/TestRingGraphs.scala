package com.nsinha.graph

import com.nsinha.graph.factories.{AssociativeNonDistributiveGraphFactory, GraphFactory, OrderedGraphFactory}
import com.nsinha.graph.interfaces.Common.{AssociativeNonDistributiveRingElem, OrderedOpaqueClass}
import com.nsinha.graph.interfaces.Ops.{AssociativeNonDistributiveGraphOps, OrderedGraphOps}
import com.nsinha.graph.interfaces._
import com.nsinha.graph.utils.dot.DotReaderImpl
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
}
