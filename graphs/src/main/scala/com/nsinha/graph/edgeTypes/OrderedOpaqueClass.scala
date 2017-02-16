package com.nsinha.graph.edgeTypes

/** Created by nsinha on 2/8/17.
  */
case class OrderedOpaqueClass(name : String) extends Ordered[OrderedOpaqueClass] {
  override def compare(that : OrderedOpaqueClass) : Int = name.compare(that.name)
}

