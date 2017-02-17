package com.nsinha.graph

import com.nsinha.graph.appConfig.ApplicationConfig
import com.nsinha.graph.factories.{GraphFactory, OrderedGraphFactory}
import com.nsinha.graph.interfaces.Common.OrderedOpaqueClass
import com.nsinha.graph.interfaces.Ops.OrderedGraphOps
import com.nsinha.graph.interfaces._
import com.nsinha.graph.utils.dot.DotReaderImpl
import org.scalatest.{FunSuite, MustMatchers}

/** Created by nsinha on 2/10/17.
  */
class TestOrderedGraph extends FunSuite with MustMatchers {

  test("test mst kruskal on test graph") {
    val _g = OrderedGraphFactory.createGraphOfOpaquesRandom(20, 1)
    val gOps = new OrderedGraphOps[OrderedOpaqueClass] {
      override val g = _g
    }
    gOps.printGraphDot("/tmp/1")
    gOps.minSpanningTree() match {
      case None ⇒
      case Some(mst) ⇒
        val mstOps = new OrderedGraphOps[OrderedOpaqueClass] {
          override val g = mst
        }
        mstOps.printGraphDot("/tmp/2")
    }

  }

  test("test mst prim on test graph") {
    val _g = OrderedGraphFactory.createGraphOfOpaquesRandom(10, 1)
    val gOps = new OrderedGraphOps[OrderedOpaqueClass] {
      override val g = _g
    }
    gOps.printGraphDot("/tmp/1")
    gOps.minSpanningTree("prim") match {
      case None ⇒
      case Some(mst) ⇒
        val mstOps = new OrderedGraphOps[OrderedOpaqueClass] {
          override val g = mst
        }
        mstOps.printGraphDot("/tmp/2")
    }

  }

}
