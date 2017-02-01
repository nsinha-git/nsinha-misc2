package com.nsinha.graph.interfaces

/**
  * Created by nsinha on 2/1/17.
  */
class Node[A](nameInit: String, override val x: Double, override val y: Double, childrenInit: List[String] = Nil, attributesInit: List[Attribute] = Nil) extends NodeTrait[A] {
  var _children = childrenInit
  var _attributes = attributesInit

  override def setAttribute(attribute: Attribute) = {
    _attributes = _attributes.:+(attribute)
  }
  override def children() = _children

  override def name = nameInit

  override def attributes = _attributes

  override def deepClone() = new Node[A](nameInit,x,y, childrenInit,attributesInit)
  override def deepClone(alternateChildren: List[String]) = new Node[A](nameInit,x,y, alternateChildren,attributesInit)
}
