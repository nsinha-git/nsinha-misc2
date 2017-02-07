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
case class Scc[A](_g : GraphTrait[A]) {
  //key is iteration
  val iThIteratedSccs : mutable.Map[Int, mutable.MutableList[SccComponent[A]]] = mutable.Map()
  //a convenience for maximal Sccs that  a node participates in
  //node> (size,List of maximal scc)

  val nodeToMaximalSccs : Map[String, mutable.Map[Int, mutable.MutableList[SccComponent[A]]]] = {
    val mp = Map[String, mutable.Map[Int, List[SccComponent[A]]]]()
    _g.nodes.foldLeft(Map[String, mutable.Map[Int, mutable.MutableList[SccComponent[A]]]]()) { (z, node) ⇒
      z + (node.name → mutable.Map[Int, mutable.MutableList[SccComponent[A]]](1 → mutable.MutableList(SccComponent(List(node)))))
    }
  }

  val baseSccs = _g.nodes.foldLeft(List[SccComponent[A]]()) { (z, el) ⇒
    val mp = nodeToMaximalSccs(el.name)
    val sccComp = SccComponent[A](List(el))
    mp.+(1 → mutable.MutableList(sccComp))
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

  val sccNameExists : mutable.Set[String] = mutable.Set()
  baseSccs map { x ⇒ sccNameExists += x.name }

  def scc() : List[SccComponent[A]] = {
    var iteration = 0
    iThIteratedSccs += (iteration → baseSccs.foldLeft(mutable.MutableList[SccComponent[A]]()) { (z, el) ⇒ z.+=(el) })

    while (iteration < _g.nodes.size) {
      val ithSccListOpt = iThIteratedSccs.get(iteration)
      ithSccListOpt match {
        case Some(ithSccList) ⇒
          //get i+1 list by taking ithSccList elements and composing on admissable  elements in NList that may work out?
          for (curIthScc ← ithSccList) {
            val nsetK = intesectionSetMap(curIthScc.name).diff(curIthScc.nodesNameSet)
            for (nsetKelem ← nsetK if sccDoesNotExist(curIthScc, nsetKelem)) {
              val nodeK = _g.getNode(nsetKelem)
              if (admissable(nodeK, curIthScc)) {
                //create a new SccComponent for K+1 stage
                val newScc = new SccComponent[A](curIthScc.m :+ nodeK)
                //record the intersection of newScc
                intesectionSetMap(newScc.name) = intesectionSetMap(curIthScc.name).intersect(intesectionSetMap(nodeK.name))
                iThIteratedSccs.get(iteration + 1) match {
                  case None    ⇒ iThIteratedSccs += ((iteration + 1) → mutable.MutableList(newScc))
                  case Some(l) ⇒ l += newScc
                }
                //record the newScc against each of their participating nodes. This is maximal for each of nodes.
                for (node ← newScc.m) {
                  val mp = nodeToMaximalSccs(node.name)
                  mp.get(newScc.m.size) match {
                    case None    ⇒ mp.+=(newScc.m.size → mutable.MutableList(newScc))
                    case Some(l) ⇒ l.+=(newScc)
                  }
                }
                //record this scc name as already processed
                sccNameExists += (newScc.name)
              }
              else {}
            }
          }
        case None ⇒
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
    }.toSet.toList
  }

  def sccDoesNotExist(curIthScc : SccComponent[A], nsetKelem : String) : Boolean = {
    val possibleName = (curIthScc.nodesNameSet.toList :+ nsetKelem).sorted.mkString("-")
    if (sccNameExists.contains(possibleName))
      false
    else
      true
  }

  def admissable(node : NodeTrait, curIthScc : SccComponent[A]) : Boolean = {
    (intesectionSetMap(node.name).intersect(curIthScc.nodesNameSet).size == curIthScc.nodesNameSet.size)
  }
}

