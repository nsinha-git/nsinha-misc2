package com.nsinha.problems.Ladder

import scala.collection.mutable

/** Created by nsinha on 4/17/17.
  */

case class FindCycle(g : Map[Node, List[Node]]) {
  /* at max o(2e+n) */
  def isValid : Boolean = {
    val mostSeniorAncestorSet = findMostSeniorAncestors //o(e)
    mostSeniorAncestorSet foreach { head ⇒ //every head is orthogonal
      val res = checkCycles(head)
      if (!res) return false
    } //total is o(e+n)

    return true
  }

  def checkCycles(curNode : Node) : Boolean = {
    val enQuedAlreadyOnThisIter = mutable.HashSet[Node](curNode)
    val res = doBfsWithAggregatedAncestorQueAndCurrentLevelQue(mutable.Queue[Node](curNode), enQuedAlreadyOnThisIter)
    if (res == false) return false
    return true
  }

  def doBfsWithAggregatedAncestorQueAndCurrentLevelQue(bfsQue : mutable.Queue[Node], enQuedAlready : mutable.Set[Node]) : Boolean = {
    var curQue = bfsQue //curBfslevel to be read
    var nextQueSet = mutable.HashSet[Node]() //nextbfslevel to be written

    while (curQue.nonEmpty && nextQueSet.nonEmpty) {
      val tempQueSet = curQue.toSet
      while (curQue.nonEmpty) {
        val curNode = curQue.dequeue()
        val nbrs = g(curNode)
        nbrs foreach { nbr ⇒
          if (enQuedAlready.contains(nbr)) return false
          if (!tempQueSet.contains(nbr)) nextQueSet += nbr
        }
      }
      //curQue is turned empty
      if (nextQueSet.nonEmpty) {
        nextQueSet foreach { el ⇒
          curQue += el
          enQuedAlready += el
        }
      }
      nextQueSet.clear()
    }
    return true
  }

  def findMostSeniorAncestors : mutable.Set[Node] = { //o(e)
    val nodeWithParentsSet = mutable.Set[Node]()
    g foreach {
      case (node, nbrs) ⇒
        nbrs foreach (nbr ⇒ nodeWithParentsSet += nbr)
    }
    val setOfNoParentsNodes = g.keySet.diff(nodeWithParentsSet)
    setOfNoParentsNodes.foldLeft(mutable.Set[Node]()) { (Z, el) ⇒ Z += el }
  }
}

