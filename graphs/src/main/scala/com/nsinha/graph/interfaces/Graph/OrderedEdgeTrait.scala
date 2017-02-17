package com.nsinha.graph.interfaces.Graph

import com.nsinha.graph.interfaces.Common.{RingElem, Weight}

/** Created by nsinha on 2/16/17.
  */
trait OrderedEdgeTrait[A] extends Ordered[OrderedEdgeTrait[A]] {
  val name : (String, String)
  val isDirected : Boolean
  val weight : Weight[A]
  def compare(that : OrderedEdgeTrait[A]) : Int = ???
}

