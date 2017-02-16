package com.nsinha.graph.interfaces.Ops

import com.nsinha.graph.interfaces.Graph.{NodeTrait, TreeTrait}

import scala.collection.mutable

/** Created by nsinha on 2/8/17.
  */
trait TreeOpsTrait[A] extends GraphOpsTrait[A] {
  val tree : TreeTrait[A]

  def createLevelsOnBfs : List[List[NodeTrait]] = {
    val root = tree.rootNode
    val q0 = new mutable.Queue[NodeTrait]()
    q0.enqueue(root)
    val q1 = new mutable.Queue[NodeTrait]()
    var qval = 0
    val qList = new mutable.Queue[List[NodeTrait]]()

    while (q0.nonEmpty || q1.nonEmpty) {
      val qToDeque = if (qval == 0) q0 else q1
      val qToFill = if (qval == 1) q0 else q1
      val newList = mutable.MutableList[NodeTrait] ()

      while (qToDeque.nonEmpty) {
        val nextElem = qToDeque.dequeue()
        newList += (nextElem)
        val children = nextElem.children() map (x ⇒ tree.graph.getNode(x))
        children foreach (qToFill.enqueue(_))
      }

      qval = (qval + 1) % 2
      qList.enqueue(newList.toList)
    }

    qList toList
  }

  def createAPreOrderedList : List[NodeTrait] = {
    //a preordered list
    val root = tree.rootNode

    createAPreOrderedListInt(root, new mutable.Queue[NodeTrait]())

  }

  def createAPreOrderedListInt(node : NodeTrait, que : mutable.Queue[NodeTrait]) : List[NodeTrait] = {
    if (!que.contains(node)) {
      val children = node.children()
      que.+=(node)
      children.foldLeft(List(node)) { (z, el) ⇒
        if (que.contains(el)) {
          z
        }
        else {
          z ++ createAPreOrderedListInt(g.getNode(el), que)
        }
      }
    }
    else {
      Nil
    }
  }

  def createAPostOrderedList : List[NodeTrait] = {
    //a postordered list
    val root = tree.rootNode
    createAPostOrderedListInt(root, new mutable.Queue[NodeTrait]())

  }

  def createAPostOrderedListInt(node : NodeTrait, que : mutable.Queue[NodeTrait]) : List[NodeTrait] = {
    if (!que.contains(node)) {
      val children = node.children()
      que.+=(node)
      children.foldLeft(List[NodeTrait]()) { (z, el) ⇒
        if (que.contains(el)) {
          z
        }
        else {
          z ++ createAPostOrderedListInt(g.getNode(el), que)
        }
      } ++ List(node)
    }
    else Nil
  }
}
