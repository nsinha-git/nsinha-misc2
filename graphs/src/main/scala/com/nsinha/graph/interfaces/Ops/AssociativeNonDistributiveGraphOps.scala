package com.nsinha.graph.interfaces.Ops

import com.nsinha.graph.interfaces.Common.RingElem

/** Created by nsinha on 2/8/17.
  */
trait AssociativeNonDistributiveGraphOps[A <: RingElem[A]] extends OrderedGraphOps[A] {
  def findMinDistSrcDest(src : String, dest : String, g : G) : Option[A] = ???

  def findMinDistForEachSrcDest(g : G) : List[(String, String, Option[A])] = ???

  def findMaxFlowSrcDest(src : String, dest : String, g : G) : A = ???

  def findMaxFlowForEachSrcDest(g : G) : List[(String, String, A)] = ???
}
