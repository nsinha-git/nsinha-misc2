package com.nsinha.graph.interfaces.Ops

import com.nsinha.graph.algorithms.ShortestPath.AllSrcShortestPath
import com.nsinha.graph.interfaces.Common.RingElem

/** Created by nsinha on 2/8/17.
  */
trait AssociativeNonDistributiveGraphOps[A <: RingElem[A]] extends OrderedGraphOps[A] {
  implicit val Zero : A
  def findMinDistSrcDest(src : String, dest : String) : Option[A] = ???

  def findMinDistForEachSrcDest : List[(String, String, List[String], A)] = {
    { new AllSrcShortestPath[A](g) }.getAllShortestPaths
  }

  def findMaxFlowSrcDest(src : String, dest : String) : A = ???

  def findMaxFlowForEachSrcDest : List[(String, String, A)] = ???
}
