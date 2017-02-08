package com.nsinha.graph.algorithms.Bipartite

import com.nsinha.graph.interfaces.{GraphOpsTrait, GraphTrait, NodeTrait, TreeOpsTrait}

import scala.collection.mutable

/** Created by nsinha on 2/8/17.
  */
class Bipartite[A](_g : GraphTrait[A]) {
  val gOps : GraphOpsTrait[A] = new GraphOpsTrait[A] {

    override val g : G = _g
  }

  def bipart : (List[String], List[String]) = {
    var failure = false
    val l1 = mutable.MutableList[String]()
    val l2 = mutable.MutableList[String]()
    val processedNodes = mutable.MutableList[String] ()

    for (node ← _g.nodes if !processedNodes.contains(node.name)) {
      processedNodes += (node.name)
      val treeOpt = gOps.bfsTree(node.name)
      treeOpt match {
        case None ⇒
        case Some(treeTrait) ⇒
          val treeOps = new TreeOpsTrait[A] {
            override val tree = treeTrait
            override val g = treeTrait.graph
          }
          val allLevels = treeOps.createLevelsOnBfs
          val (_, list1, list2) = allLevels.foldLeft((0, List[String](), List[String]())) {
            (z, el) ⇒
              if (z._1 == 0) {
                val newZ2 = el.foldLeft(z._2) {
                  (zz, elel) ⇒
                    zz :+ elel.name
                }
                (1, newZ2, z._3)
              }
              else {
                val newZ3 = el.foldLeft(z._3) {
                  (zz, elel) ⇒
                    zz :+ elel.name
                }
                (0, z._2, newZ3)
              }
          }

          if (list1.toSet.intersect(l1.toSet).isEmpty & list2.toSet.intersect(l1.toSet).isEmpty & list1.toSet.intersect(l2.toSet).isEmpty & list2.toSet.intersect(l2.toSet).isEmpty) {
            list1 foreach (x ⇒ if (!l1.contains(x)) { l1 += x })
            list2 foreach (x ⇒ if (!l2.contains(x)) { l2 += x })
          }
          else {
            if ((!list1.toSet.intersect(l1.toSet).isEmpty & !list1.toSet.intersect(l2.toSet).isEmpty) ||
              (!list1.toSet.intersect(l2.toSet).isEmpty & !list1.toSet.intersect(l1.toSet).isEmpty)) {
              failure = true
            }
            else {
              if (!list1.toSet.intersect(l1.toSet).isEmpty) {
                list1 foreach (x ⇒ if (!l1.contains(x)) {
                  l1 += x
                })
                list2 foreach (x ⇒ if (!l2.contains(x)) {
                  l2 += x
                })
              }
              if (!list2.toSet.intersect(l1.toSet).isEmpty) {
                list2 foreach (x ⇒ if (!l1.contains(x)) {
                  l1 += x
                })
                list1 foreach (x ⇒ if (!l2.contains(x)) {
                  l2 += x
                })
              }
            }
          }
      }
    }
    failure match {
      case true ⇒ (Nil, Nil)
      case false ⇒
        (l1.toSet.toList, l2.toSet.toList)
    }
  }

}
