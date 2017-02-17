package com.nsinha.graph.interfaces.Graph

import com.nsinha.graph.interfaces.Common.Attribute

/** Created by nsinha on 2/16/17.
  */
trait NodeTrait {
  val x : Double = 0
  val y : Double = 0
  def children() : List[String]
  def name : String
  def attributes : List[Attribute]
  def setAttribute(attribute : Attribute)
  def deepClone() : NodeTrait
  def deepClone(alternateChildren : List[String]) : NodeTrait
}
