package com.nsinha.graph.interfaces.Ops

import com.nsinha.graph.algorithms.MaxFlow.{MaxFlow}
import com.nsinha.graph.algorithms.ShortestPath.AllSrcShortestPath
import com.nsinha.graph.interfaces.Common.RingElem
import com.nsinha.graph.utils.ExternalProcess

import scala.reflect.io.File

/** Created by nsinha on 2/8/17.
  */
trait AssociativeNonDistributiveGraphOps[A <: RingElem[A]] extends OrderedGraphOps[A] {
  implicit val Zero : A

  def findMinDistSrcDest(src : String, dest : String) : Option[A] = ???

  def findMinDistForEachSrcDest : List[(String, String, List[String], A)] = {
    { new AllSrcShortestPath[A](g) }.getAllShortestPaths
  }

  def findMaxFlowSrcDest(src : String, dest : String) : Option[(A, List[(A, List[String])])] = {
    { new MaxFlow[A](g) }.getMaxFlow(src, dest)
  }

  def findMaxFlowForEachSrcDest : List[(String, String, A)] = ???

  override def printGraphDot(fileName : String = "/tmp/graph.dot") = {
    val f = new File(new java.io.File(fileName))
    val writer = f.bufferedWriter(false)
    //go to each node and write down a 2 lines for each of its neigbors with a blank line
    writer.append("digraph d {\n")

    g.nodes map {
      n ⇒ writer.append(n.name+" [label="+n.name+";"+"]\n")
    }
    g.nodes flatMap { node ⇒
      g.getEdgeWithSrc(node.name)
    } map { x ⇒
      val n1 = g.getNode(x.name._1)
      val n2 = g.getNode(x.name._2)

      writer.append(n1.name+"->"+n2.name + s"[label=${x.weight.getWeight}]"+"\n")
    }

    writer.append("}\n")
    writer.close()
    //call a gnuplot process
    ExternalProcess(Seq("/usr/local/bin/dot", "-Tpng", "-O", fileName))
    ExternalProcess(Seq("/usr/bin/open", s"${fileName}.png"))
  }

}
