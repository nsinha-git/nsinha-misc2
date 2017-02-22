package com.nsinha.graph.interfaces.Graph

import com.nsinha.graph.interfaces.Common.{RingElem, Weight}

/** Created by nsinha on 2/16/17.
  */
trait OrderedEdgeTrait[A] extends Ordered[OrderedEdgeTrait[A]] {
  var name : (String, String)
  var isDirected : Boolean
  var weight : Weight[A]
  def compare(that : OrderedEdgeTrait[A]) : Int = ???
}

