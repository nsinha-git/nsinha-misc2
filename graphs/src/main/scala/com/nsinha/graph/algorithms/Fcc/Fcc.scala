package com.nsinha.graph.algorithms.Fcc

import com.nsinha.graph.algorithms.ConnectedComponent
import com.nsinha.graph.interfaces.{GraphOpsTrait, GraphTrait, NodeTrait}

import scala.collection.mutable

case class Fcc[A](_g : GraphTrait[A]) {
  var steps : Long = 0L
  //key is iteration
  val iThIteratedSccs : mutable.Map[Int, mutable.MutableList[ConnectedComponent[A]]] = mutable.Map()
  //a convenience for maximal Sccs that  a node participates in
  //node> (size,List of maximal scc)

  val nodeToMaximalSccs : Map[String, mutable.Map[Int, mutable.MutableList[ConnectedComponent[A]]]] = {
    val mp = Map[String, mutable.Map[Int, List[ConnectedComponent[A]]]]()
    _g.nodes.foldLeft(Map[String, mutable.Map[Int, mutable.MutableList[ConnectedComponent[A]]]]()) { (z, node) ⇒
      steps += 1
      z + (node.name → mutable.Map[Int, mutable.MutableList[ConnectedComponent[A]]](1 → mutable.MutableList(ConnectedComponent(List(node)))))
    }
  }

  val baseSccs = _g.nodes.foldLeft(List[ConnectedComponent[A]]()) { (z, el) ⇒
    steps += 1
    val mp = nodeToMaximalSccs(el.name)
    val sccComp = ConnectedComponent[A](List(el))
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
      steps += node.children().size
    }
    mp
  }

  val sccNameExists : mutable.Set[String] = mutable.Set()
  baseSccs map { x ⇒ sccNameExists += x.name }

  def scc() : List[ConnectedComponent[A]] = {
    var iteration = 0
    iThIteratedSccs += (iteration → baseSccs.foldLeft(mutable.MutableList[ConnectedComponent[A]]()) { (z, el) ⇒ z.+=(el) })

    while (iteration < _g.nodes.size) {
      val ithSccListOpt = iThIteratedSccs.get(iteration)
      ithSccListOpt match {
        case Some(ithSccList) ⇒
          //get i+1 list by taking ithSccList elements and composing on admissable  elements in NList that may work out?
          for (curIthScc ← ithSccList) {
            val nsetK = intesectionSetMap(curIthScc.name).diff(curIthScc.nodesNameSet)
            steps += (intesectionSetMap(curIthScc.name).size + curIthScc.nodesNameSet.size)
            for (nsetKelem ← nsetK if sccDoesNotExist(curIthScc, nsetKelem)) {
              val nodeK = _g.getNode(nsetKelem)
              steps += curIthScc.nodesNameSet.size
              if (admissable(nodeK, curIthScc)) {
                //create a new SccComponent for K+1 stage
                val newScc = new ConnectedComponent[A](curIthScc.m :+ nodeK)
                steps += newScc.nodesNameSet.size
                //record the intersection of newScc
                steps += intesectionSetMap(curIthScc.name).size + intesectionSetMap(nodeK.name).size + newScc.nodesNameSet.size
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

    println("complexity "+_g.nodes.size+":"+steps)

    nodeToMaximalSccs.foldLeft(Set[ConnectedComponent[A]]()) { (z, el) ⇒
      val y = el._2.maxBy(_._1)
      y._2.foldLeft(z) { (zz, elel) ⇒
        zz + elel
      }
    }.toSet.toList

  }

  def sccDoesNotExist(curIthScc : ConnectedComponent[A], nsetKelem : String) : Boolean = {
    val possibleName = (curIthScc.nodesNameSet.toList :+ nsetKelem).sorted.mkString("-")
    if (sccNameExists.contains(possibleName))
      false
    else
      true
  }

  def admissable(node : NodeTrait, curIthScc : ConnectedComponent[A]) : Boolean = {
    (intesectionSetMap(node.name).intersect(curIthScc.nodesNameSet).size == curIthScc.nodesNameSet.size)
  }
}

