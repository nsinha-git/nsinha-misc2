package com.nsinha.graph.algorithms.scc

import com.nsinha.graph.interfaces.{GraphOpsTrait, GraphTrait, NodeTrait}

import scala.collection.mutable

/** Created by nsinha on 2/4/17.
  */

case class SccComponent[A](m : List[NodeTrait]) {
  val nodes = m.foldLeft(Map[String, NodeTrait]()) { (z, el) ⇒
    z + (el.name → el)
  }
  val name = {
    { m map (_.name) }.sorted.mkString("-")
  }
  val nodesNameSet = { m map { _.name } }.toSet

}
class Scc[A](_g : GraphTrait[A]) {
  //key is iteration
  val iThIteratedSccs : mutable.Map[Int, List[SccComponent[A]]] = mutable.Map()
  //a convenience for maximal Sccs that  a node participates in
  //node> (size,List of maximal scc)

  val nodeToMaximalSccs : Map[String, mutable.Map[Int, List[SccComponent[A]]]] = {
    val mp = Map[String, mutable.Map[Int, List[SccComponent[A]]]]()
    _g.nodes.foldLeft(Map[String, mutable.Map[Int, List[SccComponent[A]]]]()) { (z, node) ⇒
      z + (node.name → mutable.Map[Int, List[SccComponent[A]]]())
    }
  }

  val baseSccs = _g.nodes.foldLeft(List[SccComponent[A]]()) { (z, el) ⇒
    val mp = nodeToMaximalSccs(el.name)
    val sccComp = SccComponent[A](List(el))
    mp.+(1 → List(sccComp))
    z :+ sccComp
  }
  val gOps = new GraphOpsTrait[A] { override val g = _g }
  val gTranspose : GraphTrait[A] = gOps.transpose.asInstanceOf[GraphTrait[A]]
  val gTOps = new GraphOpsTrait[A] { override val g = gTranspose }
  val intesectionSetMap = {
    val mp = mutable.Map[String, Set[String]]()
    _g.nodes foreach { node ⇒
      mp.+=((node.name → node.children().toSet))
    }
    mp
  }

  def scc() : List[SccComponent[A]] = {
    var iteration = 0
    iThIteratedSccs += (iteration → baseSccs)

    while (iteration < _g.nodes.size) {
      val ithSccList = iThIteratedSccs(iteration)
      if (ithSccList.isEmpty) {} else {
        //get i+1 list by taking ithSccList elements and composing on admissable  elements in NList that may work out?
        for (curIthScc ← ithSccList) {
          val nsetK = intesectionSetMap(curIthScc.name).diff(curIthScc.nodesNameSet)
          for (nsetKelem ← nsetK) {
            val nodeK = _g.getNode(nsetKelem)
            if (admissable(nodeK, curIthScc)) {
              //create a new SccComponent for K+1 stage
              val newScc = new SccComponent[A](curIthScc.m :+ nodeK)
              //record the intersection of newScc
              intesectionSetMap(newScc.name) = intesectionSetMap(curIthScc.name).intersect(intesectionSetMap(nodeK.name))
              iThIteratedSccs.get(iteration + 1) match {
                case None    ⇒ iThIteratedSccs += ((iteration + 1) → List(newScc))
                case Some(l) ⇒ l :+ newScc
              }
              //record the newScc against each of their participating nodes. This is maximal for each of nodes.
              for (node ← newScc.m) {
                val mp = nodeToMaximalSccs(node.name)
                mp.get(newScc.m.size) match {
                  case None    ⇒ mp.+=(newScc.m.size → List(newScc))
                  case Some(l) ⇒ l.:+(newScc)
                }
              }
            }
            else {}
          }
        }
      }
      iteration += 1
    }

    //after all iterations
    //visit all nodes and collect their maximal Scc into a common set. This set must be maximal for G as we cover all nodes maximal.
    // if there is a maximal for a node it must exist in nodes table and thus must be part of this Set.
    // x \member NodeMaximal => x \member ThisSet
    // x \member ThisSet => x \memeber NodeMaximal(some node)

    nodeToMaximalSccs.foldLeft(Set[SccComponent[A]]()) { (z, el) ⇒

      val y = el._2.maxBy(_._1)
      y._2.foldLeft(z) { (zz, elel) ⇒
        zz + elel
      }
    }.toList
  }

  def admissable(node : NodeTrait, curIthScc : SccComponent[A]) : Boolean = {
    (intesectionSetMap(node.name).intersect(curIthScc.nodesNameSet).size == curIthScc.nodesNameSet.size)
  }
}

