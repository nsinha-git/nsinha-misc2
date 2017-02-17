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
    }
    gOps.printGraphDot("/tmp/1")
  }

}
