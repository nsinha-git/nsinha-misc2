package com.nsinha.problems.Ladder

import scala.collection.immutable.HashSet
import scala.collection.mutable

/** Created by nsinha on 4/17/17.
  */
case class Node(str : String) {
  def apply : String = str
  override def hashCode() = str.hashCode
}

case class FindCycle2(g : Map[Node, List[Node]]) {
  /* at max o(2e+n) */
  def isValid : Boolean = {
    val mostSeniorAncestorSet = findMostSeniorAncestors //o(e)

    if (mostSeniorAncestorSet.isEmpty) return false

    mostSeniorAncestorSet foreach { head ⇒ //every seniormost ancestor  atleast vists one univsited node(itself)
      val res = checkCycles(head)
      if (!res) return false
    }

    return true
  }

  def checkCycles(curNode : Node) : Boolean = {
    return doDfsWithPathVisitedLogged(curNode, HashSet[Node](curNode), mutable.HashSet[Node](curNode))
  }

  def doDfsWithPathVisitedLogged(curNode : Node, visitedCurrentPathNodes : Set[Node], universalVisitedNodeSet : mutable.Set[Node]) : Boolean = {
    val nbrs = g(curNode)
    val curNodeVisitedPathNodes = visitedCurrentPathNodes.foldLeft(mutable.HashSet[Node]()) { (Z, el) ⇒ Z += el }
    nbrs foreach { nbr ⇒ //none of children should point to any in parent prefix path
      if (visitedCurrentPathNodes.contains(nbr)) return false
    }
    //visit each of kid now if not in universal visited list
    nbrs foreach { nbr ⇒
      if (!universalVisitedNodeSet.contains(nbr)) {
        universalVisitedNodeSet += nbr
        val res = doDfsWithPathVisitedLogged(nbr, visitedCurrentPathNodes + nbr, universalVisitedNodeSet)
        if (!res) return false
      }
    }
    return true
  }

  def findMostSeniorAncestors : mutable.Set[Node] = {
    //o(e)
    val nodeWithParentsSet = mutable.Set[Node]()
    g foreach {
      case (node, nbrs) ⇒
        nbrs foreach (nbr ⇒ nodeWithParentsSet += nbr)
    }
    val setOfNoParentsNodes = g.keySet.diff(nodeWithParentsSet)
    setOfNoParentsNodes.foldLeft(mutable.Set[Node]()) { (Z, el) ⇒ Z += el }
  }

}

/*
1.A dfs from root node visits  all nodes visitable from root upto the leaf node. We are maintaining the prefixPath to every node that got
 visited when travelling to that node.
2. If a node ever tries visiting a node on its prefix path, it is a cycle.
3. We also maintain a universal visited list. A node when visisted  ever gets enetred in this universal vsited list.
4. Imagine a current node visiting some destination node which is already in universal list. The destiation node dfs
 can never  visit back the current node because in that case by property of dfs the current node would already have been visited.

 So there is no need to vsit the already univeral list visited node again as it cant affect the sattus of unvisted nodes.

 We simply complete the dfs of the current node with other children in like manner.

5. In summary these cases can occur wrt to universal visit set,  prefixPathSet and curNode(curNode) and for each of its children(Ki)
 case 1: ki is in uni visited list. Ignore as ki cant posses any node that is going to cycle back any of unvisted nodes by dfs property
 case 2: ki is in prefixPathSet. A cycle is found. Fail it
 case 3: ki is not in prefix set. Update its; prefix path and visit it.

 when all ki is done/or no ki existed , finish the curNode and move up to parent in dfs path.

 Moving up will ultimately move to top ancestor and we will have visted the ancestor fully.

 Move to next ancestor till exhaustion.
6. Analysis: every node will be vsisted at most once and universalVisitSet will not allow any more than one visit. Every unvsited node will
be either a top ancestor in which will be vsisted exactly one time, or a child node in dfs of a rootancestor which then again will be vsited
once. So we get o(v).
An edge will ve visited once when finding the new node or  when revisiting an already visited node/and or cycle to prefix path. this
is o(3e) at worst.
so we have total o(3e+v)
*/

