package com.nsinha.graph.interfaces.Graph

import com.nsinha.graph.interfaces.Common.Weight

/**
  * Created by nsinha on 2/16/17.
  */
trait EdgeTrait[A] extends Ordered[EdgeTrait[A]] {
  val name : (String, String)
  val isDirected : Boolean
  val weight : Weight[A]
  def compare(that : EdgeTrait[A]) : Int = ???
}
