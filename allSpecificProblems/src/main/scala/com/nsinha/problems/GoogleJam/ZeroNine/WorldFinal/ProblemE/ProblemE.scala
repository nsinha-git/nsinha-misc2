package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemE

import com.nsinha.common.MapUtils
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/29/17.
  */

case class SpanNode(left : Int, right : Int)

case class NodeAdjacency(fullyContained : mutable.Queue[SpanNode] = mutable.Queue(), partialIntersection : mutable.Queue[SpanNode] = mutable.Queue(), parents : mutable.Queue[SpanNode] = mutable.Queue())
case class ProblemE(input : String) {
  type Pos = Int
  type Boundary = Int
  val leftBoundaryAscMap = mutable.HashMap[String, Pos]()
  val leftBoundaryAscQueue = mutable.Queue[String]()
  val rightBoundaryAscMap = mutable.HashMap[String, Pos]()
  val rightBoundaryAscQueue = mutable.Queue[String]()
  val leftRightBoundaryAscQueue = mutable.Queue[(String, Pos, Boundary)]()
  val allSpanNode : Map[String, SpanNode] = getSpanNodes
  val allSpanNodeInvert : Map[SpanNode, String] = MapUtils.invertMap[String, SpanNode](allSpanNode)
  val spanNodeGraph : mutable.Map[SpanNode, NodeAdjacency] = createEdges

  val spanNodeColor : mutable.Map[SpanNode, Int] = allSpanNode.foldLeft(mutable.HashMap[SpanNode, Int]()) { (Z, el) ⇒
    Z += el._2 → 0
  }

  //println({ HeightForGraph(spanNodeGraph, allSpanNode) }.solve)

  HyperGraph(spanNodeGraph.toMap, spanNodeColor)

  def getSpanNodes : Map[String, SpanNode] = {
    val nodeBoundaries = input.split(" ")
    val nodeBoundariesMap = mutable.HashMap[String, mutable.Queue[Pos]] ()

    nodeBoundaries zip Range(0, nodeBoundaries.size) foreach { col ⇒
      if (!nodeBoundariesMap.contains(col._1)) nodeBoundariesMap += col._1 → mutable.Queue[Pos]()
      nodeBoundariesMap(col._1) += col._2
      if (nodeBoundariesMap(col._1).size == 1) {
        leftBoundaryAscMap += col._1 → col._2
        leftBoundaryAscQueue += col._1
        leftRightBoundaryAscQueue += ((col._1, col._2, 0))
      }
      else {
        rightBoundaryAscMap += col._1 → col._2
        rightBoundaryAscQueue += col._1
        leftRightBoundaryAscQueue += ((col._1, col._2, 1))
      }
    }

    nodeBoundariesMap map { x ⇒
      val q = x._2
      x._1 → SpanNode(q.dequeue(), q.dequeue())
    } toMap
  }

  def createEdges : mutable.Map[SpanNode, NodeAdjacency] = {
    val mapOfNodes = mutable.HashMap[SpanNode, NodeAdjacency]()

    allSpanNode foreach { case (_, node) ⇒ mapOfNodes += node → NodeAdjacency() }

    val queueForLeftOrder = mutable.Buffer[String]()
    /* these ops must be supported by this Data struct.
    1. insert the left pos for any node.(A que like interface)
    2. when seeing the right pos remove the left pos optimally.(a hashmap that contain to_remove_pos and a tree for logn removal at pos or que backed by
     array and a bsearch and  with tombstones to save upon shifts). But looking at 3, we spent n already. So we may as well look at a list containing
       que. and travresing to left in o(n) and removing that.
    3.give a report of all nodes that fall between left and right pos. (intersects). Based on 2. when removing start a que and add  elements till the end.
    o(n)
    based on 4 we can do that but we will need to travel to left get its q , call it q1.
    we can get last entry q call it q2
    now intesects are q2 -q1 in stable order.(this could be o(n)). can we improve it. i conjecture not. as list intersection is involved.


    4.give a report of all nodes that are before left.(parents & kids) any node should contain a left ordered que of all its parents. when processing
    a left pos node just take the last entry que it's parental que and add last entry to it and calling it this node que.
    based on 2 we can start que from beginning and stop when we delete 2. o(n)

    based on thsi we can choose que as only list impl and traverse it in o(N)

     */

    for (curNode ← leftRightBoundaryAscQueue) {
      curNode._3 match {
        case 0 ⇒
          //we need to add this node to the running que
          queueForLeftOrder += curNode._1
        case 1 ⇒
          //we see right. lets traverse the list from beginning to left of this node and find all the parents and then traverse to end and find all the intersects/
          // also remove the left . o(2n) but this could have been improved to o(n) if we had c++ iterator which could have been manipulated locally
          val lookups = queueForLeftOrder.foldLeft(List[SpanNode](), List[SpanNode](), false) { (Z, el) ⇒
            val leftSeen = Z._3 || el == curNode._1
            val parents = if (!leftSeen) Z._1.:+(allSpanNode(el)) else Z._1
            val intersects = if (leftSeen & el != curNode._1) Z._2.:+(allSpanNode(el)) else Z._2
            (parents, intersects, leftSeen)
          }
          val entry = mapOfNodes(allSpanNode(curNode._1))
          lookups._1.foldLeft(entry.parents) { (Z, el) ⇒ Z += el }
          lookups._2.foldLeft(entry.partialIntersection) { (Z, el) ⇒ Z += el }
          entry.parents foreach { parent ⇒ mapOfNodes(parent).fullyContained += allSpanNode(curNode._1) }
          //partial intersection is not guaranteed to be sorted following this step but we dont use this condition
          //in our implementation
          entry.partialIntersection foreach { pInter ⇒
            if (pInter.right > allSpanNode(curNode._1).right) {
              mapOfNodes(pInter).partialIntersection += allSpanNode(curNode._1)
            }
          }
          queueForLeftOrder -= (curNode._1)
      }

    }
    mapOfNodes map { x ⇒ x._1 → x._2.copy(fullyContained = x._2.fullyContained.reverse) }
  }

  def isFullyContainedInLeftNode(nodeLeft : SpanNode, node2 : SpanNode) : Boolean = {
    //left of node2 is contained inside nodeLeft
    if (!(node2.left < nodeLeft.right & node2.left > nodeLeft.left)) return false
    //right of node2 is contained inside nodeLeft
    if (!(node2.right < nodeLeft.right & node2.right > nodeLeft.left)) return false
    return true
  }

  def isPartialIntersection(nodeLeft : SpanNode, node2 : SpanNode) : Boolean = {
    //left of node2 is contained inside nodeLeft
    val leftOrdinateInside = (node2.left < nodeLeft.right & node2.left > nodeLeft.left)
    //right of node2 is contained inside nodeLeft
    val rightOrdinateInside = (node2.right < nodeLeft.right & node2.right > nodeLeft.left)
    leftOrdinateInside || rightOrdinateInside
  }
}

case class HeightForGraph(g : Map[SpanNode, List[SpanNode]], allNodes : List[SpanNode]) {
  val allNodesSet = allNodes.toSet

  def solve : Option[Int] = {
    if (g.size == 0) return Option(0)
    val allIps = findIndependentSets
    var max = 0
    var failure = false

    allIps foreach {
      thisIp ⇒
        if (!failure) {
          val (residulaNodes : List[SpanNode], maximalNodeOpt, residualGraph) = removeMaximal(thisIp)
          maximalNodeOpt match {
            case None ⇒
              failure = true

            case Some(maximalNode) ⇒
              val thisIpHtOpt = {
                HeightForGraph(residualGraph, residulaNodes)
              }.solve
              thisIpHtOpt match {
                case None           ⇒ failure = true
                case Some(thisIpHt) ⇒ if (max < (thisIpHt + 1)) { max = thisIpHt + 1 }
              }
          }
        }
    }

    if (!failure) Option(max) else None
  }

  def removeMaximal(ipList : List[SpanNode]) : (List[SpanNode], Option[SpanNode], Map[SpanNode, List[SpanNode]]) = {
    val (nodeOpt, gCur) = getMaximal(ipList)
    nodeOpt match {
      case None ⇒ (Nil, None, gCur)
      case Some(node) ⇒ (ipList filter (x ⇒ x != node), nodeOpt, {
        gCur filter (x ⇒ x._1 != node) map { x ⇒
          x._1 → x._2.filter { y ⇒ y != node }
        }
      }.filter(x ⇒ x._2.size != 0))
    }
  }

  def getMaximal(ip : List[SpanNode]) : (Option[SpanNode], Map[SpanNode, List[SpanNode]]) = {
    val ipSet = ip.toSet
    val sizeToCheck = ip.size - 1
    val gCur = g filter (x ⇒ ipSet.contains(x._1)) map { x ⇒ x._1 → x._2.filter (y ⇒ ipSet.contains(y)) }

    val maximals = mutable.MutableList[SpanNode]()

    ip foreach {
      x ⇒
        if (gCur(x).size == sizeToCheck) {
          maximals += x
        }
    }

    if (maximals.isEmpty) {
      (None, gCur)
    }
    else if (maximals.size > 1) {
      (maximals.sortBy(x ⇒ x.left - x.right).headOption, gCur)
    }
    else {
      (maximals.headOption, gCur)
    }
  }

  def findIndependentSets : List[List[SpanNode]] = {
    val visited = mutable.HashSet[SpanNode]()
    val unvisited = mutable.HashSet[SpanNode]()
    val ipSets = mutable.MutableList[List[SpanNode]]()

    allNodes foreach (node ⇒ unvisited += node)

    while (unvisited.nonEmpty) {
      val curNodeToVisit = unvisited.head
      val l : List[SpanNode] = doSearch(curNodeToVisit)
      l foreach { x ⇒
        unvisited.remove(x)
        visited.add(x)
      }
      ipSets += l
    }

    ipSets.toList
  }

  def doSearch(curNode : SpanNode) : List[SpanNode] = {
    val visited = mutable.HashSet[SpanNode]()
    doDfs(visited, curNode)
    visited.toList
  }

  def doDfs(visited : mutable.HashSet[SpanNode], curNode : SpanNode) : Unit = {
    visited += curNode

    val nbrs = g(curNode).toSet.intersect(allNodesSet)

    nbrs foreach (nbr ⇒ if (!visited.contains(nbr)) doDfs(visited, nbr))
  }

}

case class Dir(n : Int)
case class GraphSet(g : Map[SpanNode, (Dir, List[SpanNode])])

class GraphBase(graphSet : GraphSet) {
  val g = graphSet.g
  val allNodes = g.keys.toList
  val allNodesSet = g.keySet

  def findIndependentSets : List[GraphSet] = {
    val visited = mutable.HashSet[SpanNode]()
    val unvisited = mutable.HashSet[SpanNode]()
    val ipSets = mutable.MutableList[List[SpanNode]]()

    allNodes foreach (node ⇒ unvisited += node)

    while (unvisited.nonEmpty) {
      val curNodeToVisit = unvisited.head
      val l : List[SpanNode] = doSearch(curNodeToVisit)
      l foreach { x ⇒
        unvisited.remove(x)
        visited.add(x)
      }
      ipSets += l
    }

    val res = ipSets map { elem ⇒
      g filter (x ⇒ elem.contains(x._1)) map { x ⇒ x._1 → (x._2._1, x._2._2 filter (y ⇒ elem.contains(y))) }
    }
    res.toList map { GraphSet(_) }
  }

  def doSearch(curNode : SpanNode) : List[SpanNode] = {
    val visited = mutable.HashSet[SpanNode]()
    doDfs(visited, curNode)
    visited.toList
  }

  def doDfs(visited : mutable.HashSet[SpanNode], curNode : SpanNode) : Unit = {
    visited += curNode

    val nbrs = g(curNode)._2.toSet.intersect(allNodesSet)

    nbrs foreach (nbr ⇒ if (!visited.contains(nbr)) doDfs(visited, nbr))
  }

  def isPartialIntersection(node1 : SpanNode, node2 : SpanNode) : Boolean = {
    val span1 = node1.right - node1.left
    val span2 = node2.right - node2.left

    val (larger, smaller) = if (span1 > span2) (node1, node2) else (node2, node1)

    if (smaller.left < larger.left) {
      if (smaller.right > larger.left) return true
    }
    else if (smaller.left == larger.left) {
      if (smaller.right <= larger.right) {
        return false
      }
      else {
        return true
      }
    }
    else {
      if (smaller.right > larger.right) {
        return true
      }
      else return true
    }
    false
  }

  def findMaximalNodesOnConnectedGraph() : (Option[SpanNode], Option[SpanNode]) = {
    if (allNodes.isEmpty) return (None, None)
    val allNodesSorted = allNodes.sortBy(node ⇒ -(node.right - node.left))

    val firstMaximalNode = allNodesSorted.head
    val allNodesSortedAfterFirstMaximal = allNodesSorted.drop(1)

    if (allNodesSortedAfterFirstMaximal.isEmpty) return (Option(firstMaximalNode), None)
    var secondMaximalNodeOpt : Option[SpanNode] = None
    for (node ← allNodesSortedAfterFirstMaximal) {
      if (secondMaximalNodeOpt == None && isPartialIntersection(node, firstMaximalNode)) {
        secondMaximalNodeOpt = Option(node)
      }
    }

    (Option(firstMaximalNode), secondMaximalNodeOpt)
  }

}

case class HyperGraph(g : Map[SpanNode, NodeAdjacency], gColor : mutable.Map[SpanNode, Int]) {
  type Height = Int
  var maxHeight : Height = 0
  var failure = false
  var failureOfBiPartite = false
  val heightDpMap = mutable.HashMap[SpanNode, Array[(Height, Height)]]()
  processTheDfsOfHeight

  private def findFarthestParentNode(node : SpanNode) : SpanNode = { if (g(node).parents.isEmpty) node else findFarthestParentNode(g(node).parents.head) }

  def processTheDfsOfHeight : Height = {
    // the nodes in coloredNodeBaseGraph can form a forest. so we need to be able to subtract after we visit each of trees.
    // we can create a mutable set of all nodes to start with. We will subtract a node from this set as we finish off with that node.
    //we will keep prcoessing ad updating Height till all of set is exhausted.

    val nodesQueUnvisited = g.foldLeft(mutable.Set[SpanNode]()) { (Z, el) ⇒ Z += el._1 }
    while (nodesQueUnvisited.nonEmpty) {
      val curNode = findFarthestParentNode(nodesQueUnvisited.head) //prove that a unvisited node farthest parent would be always unvisited in dfs
      doDfsHeightTop(curNode, nodesQueUnvisited)
    }

    println(maxHeight)
    maxHeight
  }

  def doDfsHeightTop(curNode : SpanNode, unVisited : mutable.Set[SpanNode]) : Unit = {
    if (heightDpMap.contains(curNode)) return
    assert(unVisited.contains(curNode))
    unVisited.remove(curNode)
    doDfsHeight(curNode, unVisited)
  }

  def doDfsHeight(curNode : SpanNode, unVisited : mutable.Set[SpanNode]) : Array[(Height, Height)] = {
    val curNodeIntersection = g(curNode).partialIntersection
    val curNodeIntersectionUnvisited = curNodeIntersection filter (x ⇒ unVisited.contains(x))
    val isAllIntersectionsUnVisited = curNodeIntersection forall (x ⇒ unVisited.contains(x)) // true only when all of interscets are still unvisited. we make this node aggregator

    if (curNodeIntersection.nonEmpty) {
      val curColor = gColor(curNode)
      if (curColor == 0) {
        gColor(curNode) = 1
        curNodeIntersection foreach (x ⇒ if (gColor(x) == 0) gColor(x) = 2 else if (gColor(x) == 2) assert(true) else assert(false))
      }
      else if (curColor == 1) {
        curNodeIntersection foreach (x ⇒ if (gColor(x) == 0) gColor(x) = 2 else if (gColor(x) == 1) assert(false) else assert(true))
      }
      else { //color is 2
        curNodeIntersection foreach (x ⇒ if (gColor(x) == 0) gColor(x) = 1 else if (gColor(x) == 1) assert(true) else assert(false))
      }
    }
    val color = gColor(curNode)
    val curNodeChildren = g(curNode).fullyContained
    val curNodeChildrenUnvisited = curNodeChildren filter { x ⇒ unVisited.contains(x) }
    //prove we come to child only through their parents and preBiases is going to be good.
    val preBiases = findPreBiasesForThisNodeBasedOnParents(curNode)

    val heightArrayFromFullyContainedChildren : Array[(Height, Height)] = if (curNodeChildren.isEmpty) {
      color match {
        case 0 ⇒ Array[(Height, Height)]((0, 1), (1, 0))
        case 1 ⇒ Array[(Height, Height)]((1, 0))
        case 2 ⇒ Array[(Height, Height)]((0, 1))
        case _ ⇒ throw new RuntimeException()
      }
    }
    else if (curNodeChildrenUnvisited.isEmpty) {
      //all children have been visited
      //get all children and find their height pairs
      val childHeightsTop = curNodeChildren map { el ⇒ heightDpMap(el) }
      findBestHeightsFromChildren(childHeightsTop.toSet, color, preBiases)
    }
    else {
      //there must be children who remain unvisited. unconditionally visit everyone from here
      curNodeChildrenUnvisited foreach (doDfsHeightTop(_, unVisited))
      val childHeightsTop = curNodeChildren map { el ⇒ heightDpMap(el) }
      findBestHeightsFromChildren(childHeightsTop.toSet, color, preBiases)
    }

    //at this point we have a height from all immediate children which were unconsrtrained from  parent. So this is the best childHeight we will ever get

    //now we can look at the intersecting nodes with this node. These nodes can be also visited in unconstarined manner. Once we get the result we need to
    //only choose those that can combine with heightArrayFromFullyContainedChildren

    if (curNodeIntersection.isEmpty) {
    }
    else if (!isAllIntersectionsUnVisited) {
      //there are intersections but but atleast one of them is already getting visited. We can show that this can happen when one of intersection is still getting visited. In this
      //case we can treat as similar to as if no intersection existed and let the first span worry about rest of intersects
    }
    else { //top and first intersect
      //there are some intersections not yet visited we should visit them in constrained manner and make this span as top node for aggregation.
      curNodeIntersectionUnvisited foreach (doDfsHeightTop(_, unVisited)) // 2 means -y direction is allowed for the top node
    }

    //at this stage we expect the intersects to be fully populated in DpTable except for top intersect which started the process
    //and the DpTable will not contain an entry for that just yet as visit of that is not over. Remember Dpentry is done only when
    //visit is fully complete.
    val isThisTheFirstIntersectProcessed : Boolean = if (curNodeIntersection.isEmpty) false else curNodeIntersection forall (x ⇒ heightDpMap.contains(x))

    if (isThisTheFirstIntersectProcessed) {
      //all intersects have been traversed . This must be the controlling intersect and belong to non-empty set of intersects
      val heightArrayFromIntersects : Array[(Height, Height)] = {
        val intersectsHeightsTop = curNodeIntersection map { el ⇒ heightDpMap(el) }
        //find a grandestParents of intersecting elements . too bad if not a singleParent
        val grandestParentOfIntersectionSet = curNodeIntersection.foldLeft(Set[SpanNode]()) { (Z, el) ⇒ Z + findFarthestParentNode(el) }
        if (grandestParentOfIntersectionSet.size > 1 || grandestParentOfIntersectionSet.isEmpty) {
          assert(false)
          failure = true
          Array()
        }
        else {
          //the grandest parent was calculated with 2 color as with any other intersect. We now need to choose the value from grandestIntersectParent and this node own's
          //fullyContainedChildren that will fit the bill.
          // we have two arrays of height options for this node. one for its direct kids and other for its intersection.
          //the heights are already constarined in two separate direcctions
          //we just expect the array of each to contain a single entry
          assert(heightArrayFromFullyContainedChildren.size == 1)
          println(s"heightDpMap populated at key $curNode")
          heightDpMap += curNode → heightArrayFromFullyContainedChildren
          heightArrayFromFullyContainedChildren foreach { el ⇒
            if (el._1 > maxHeight) maxHeight = el._1
            if (el._2 > maxHeight) maxHeight = el._2
          }
          heightArrayFromFullyContainedChildren
        }
      }
      heightArrayFromFullyContainedChildren
    }
    else {
      println(s"heightDpMap populated at key $curNode")
      heightDpMap += curNode → heightArrayFromFullyContainedChildren
      heightArrayFromFullyContainedChildren foreach { el ⇒
        if (el._1 > maxHeight) maxHeight = el._1
        if (el._2 > maxHeight) maxHeight = el._2
      }
      heightArrayFromFullyContainedChildren
    }
  }

  def findPreBiasesForThisNodeBasedOnParents(curNode : SpanNode) : (Height, Height) = {
    //this code assumes we decide on color of parents before we proceed to kids
    val allParents = g(curNode).parents
    val allOnes = { allParents filter (gColor(_) == 1) }.size
    val allTwos = { allParents filter (gColor(_) == 2) }.size
    (allOnes, allTwos)
  }

  def findBestHeightsFromChildren(childrenHeightsIn : Set[Array[(Height, Height)]], color : Int, preBiases : (Height, Height)) : Array[(Height, Height)] = {
    val childrenHeightsAfterBiasing = childrenHeightsIn map { x ⇒ x map { y ⇒ (y._1 + preBiases._1, y._2 + preBiases._2) } }
    val minXLowerBound = findMaxOfMinOnListOfTuples(childrenHeightsAfterBiasing, 0)
    val minYLowerBound = findMaxOfMinOnListOfTuples(childrenHeightsAfterBiasing, 1)

    val childHeightsTopConditionedOnX = filterArrayBasedOnAxis(childrenHeightsAfterBiasing, 0, minXLowerBound)
    val childHeightsTopConditionedOnY = filterArrayBasedOnAxis(childrenHeightsAfterBiasing, 1, minYLowerBound)

    val maxYForMinXBound = {
      childHeightsTopConditionedOnX.map(_._2)
    }.max
    val maxXForMinYBound = {
      childHeightsTopConditionedOnY.map(_._1)
    }.max

    //(minX, maxYCOn) and (maxXCond, minY) are two options
    val xGroupTuple = (minXLowerBound, maxYForMinXBound)
    val yGroupTuple = (maxXForMinYBound, minYLowerBound)
    val maxXgroup = Math.max(minXLowerBound, maxYForMinXBound)
    val minXgroup = Math.min(minXLowerBound, maxYForMinXBound)
    val maxYgroup = Math.max(minYLowerBound, maxXForMinYBound)
    val minYgroup = Math.min(minYLowerBound, maxXForMinYBound)

    val preResTupleArray : Array[(Height, Height)] = if (maxXgroup == maxYgroup) {
      Array[(Height, Height)](xGroupTuple, yGroupTuple)
    }
    else if (maxXgroup > maxYgroup) {
      Array[(Height, Height)](yGroupTuple)
    }
    else {
      Array[(Height, Height)](xGroupTuple)
    }

    val resArrayUnfiltered : Array[(Height, Height)] = color match {
      case 0 ⇒
        val res = preResTupleArray flatMap { preResTuple ⇒
          if (preResTuple._1 == preResTuple._2) {
            Array((preResTuple._1 + 1, preResTuple._2), (preResTuple._1, preResTuple._2 + 1))
          }
          else if (preResTuple._1 > preResTuple._2) {
            Array((preResTuple._1, preResTuple._2 + 1))
          }
          else {
            Array((preResTuple._1 + 1, preResTuple._2))
          }
        }
        res
      case 1 ⇒ preResTupleArray flatMap { preResTuple ⇒ Array((preResTuple._1 + 1, preResTuple._2)) }
      case 2 ⇒ preResTupleArray flatMap { preResTuple ⇒ Array((preResTuple._1, preResTuple._2 + 1)) }
      case _ ⇒ throw new RuntimeException()
    }

    val minHeight = { resArrayUnfiltered map (x ⇒ Math.max(x._1, x._2)) }.min

    val returnArray = { resArrayUnfiltered filter { x ⇒ Math.max(x._1, x._2) <= minHeight } }.toSet[(Height, Height)].toArray[(Height, Height)]
    val minSum = { returnArray map { x ⇒ x._1 + x._2 } }.min

    //remove preBiasing
    { returnArray filter { x ⇒ (x._1 + x._2) == minSum } } map { x ⇒ (x._1 - preBiases._1, x._2 - preBiases._2) }

  }

  private def filterArrayBasedOnAxis(ll : Set[Array[(Height, Height)]], axis : Int, bound : Height) : List[(Height, Height)] = {
    val otherAxis = if (axis == 0) 1 else 0

    //only keep those elems that can survive the bound i.e their axis has a value lower or equal to

    val intermediateLL = ll map { arrEl ⇒

      arrEl.size match {
        case 1 ⇒
          val firstEl = arrEl(0).productIterator.map (_.asInstanceOf[Height]).toArray[Height]
          if (firstEl(axis) > bound) throw new RuntimeException()
          arrEl(0)
        case 2 ⇒
          val firstEl = arrEl(0).productIterator.map (_.asInstanceOf[Height]).toArray[Height]
          val secondEl = arrEl(1).productIterator.map (_.asInstanceOf[Height]).toArray[Height]
          val winner = if (firstEl(axis) > secondEl(axis)) {
            1
          }
          else if (firstEl(axis) == secondEl(axis)) {
            if (firstEl(otherAxis) < secondEl(otherAxis)) arrEl(1) else arrEl(0)
          }
          else {
            0
          }
          val winnerEl = if (winner == 0) firstEl else secondEl

          if (winnerEl(axis) > bound) throw new RuntimeException()

          if (winner == 0) arrEl(0) else arrEl(1)
        case _ ⇒ throw new RuntimeException()
      }

    }
    intermediateLL.toList
  }

  private def findMaxOfMinOnListOfTuples(ll : Set[Array[(Height, Height)]], axis : Int) : Height = {
    val otherAxis = if (axis == 0) 1 else 0
    val res = {
      ll.map { arrEl ⇒
        arrEl.size match {
          case 1 ⇒ arrEl(0)
          case 2 ⇒
            val firstEl = arrEl(0).productIterator.map (_.asInstanceOf[Height]).toArray[Height]
            val secondEl = arrEl(1).productIterator.map (_.asInstanceOf[Height]).toArray[Height]
            if (firstEl(axis) > secondEl(axis)) {
              arrEl(1)
            }
            else if (firstEl(axis) == secondEl(axis)) {
              if (firstEl(otherAxis) < secondEl(otherAxis)) arrEl(1) else arrEl(0)
            }
            else {
              arrEl(0)
            }
          case _ ⇒ throw new RuntimeException()
        }
      }
    }.foldLeft(0) { (Z, el) ⇒
      val elArray = el.productIterator.map (_.asInstanceOf[Height]).toArray[Height]
      if (Z < elArray(axis)) { elArray(axis) } else { Z }
    }
    res
  }
}

class Testing extends FunSuite {
  test("a") {
    ProblemE("red red blue yellow blue yellow ")
  }

  test("b") {
    ProblemE("a b c d d c e b f a g g f e")
  }

  test("c") {
    ProblemE("lf qj uk qn qn rm qj bj bj hr hr x wn wn di rm uk di x zg nb zd fd cl fd cl tr id iq tr ac cn ec jb es vq pq mk mk pq es vq sn in kb kb tn tq tn in hc hc tq hm si hm sn si zo zo cn xo od kf tg ct js kf js ct tg xo qd qd od ih de de ih gd jf jf jb xj ec ac xj gd bh hl rh fs wo wo rh oj fs yg bc bc oj yg dc jj dc ol w ol jj w iq id hl bh nb lf ge ge re xk yk xk yk fc zd fc zg sp sp re dd wr bp we gb nn mp ub nn et is el is el s et gb we s bp ee fj ee fj ub ld lg hk hk ld lg mp bn mi dl vh zm zm vh to yp sm yp kc fm fm kc to qm sm qm bn dl lo ng gn gn db ng uc db lo lb lb uc ke ke mi uj uj vk oe oe xp ud wf xn uo xp kj kj y am am uo mc xn fq fq wf mc ik ud ik y t pj vk dd pj ji t ji ij ij wr yj en yj pr vd jc sk eb zs ki qq ze eg uf wc uf eg ze ft ff io xq bi ko ft wc sl pl sl eb om om tj wg jk ep mo yc as ks zf as wg tj jc zf an vd en ve an ps ks yc mo ep hq wb jk pl gs me bm ab cq ce qc fg cq ab me fg qr qc ce li cm li le cm z le qr gs gj gc bs gc gp vo gp z cs bm cs op vo op bs hs hf gj gi hf gi hs ko ml wb ml rs ah vj bi zj xq al jh al zj ao jh be io ff qq xs so xs lp zp ki ef zs ef cb er xg zp lp so vp ys cj tp cj cg ys vp kr cg em kr dn em tp dn ek xh xh m fp qk be ao vj ei tc kg eq vc ag eq kg ag vc nm wh ei wh nm tc qk fp ah he m ek ap xg er ap ts qs hn zn gf zn of qs kp ts kp of gf mf hn mf cb he rs sk hq pr ps ve tl xe tl yo k sf wk ll ll j j wk do bf xi xi k bf do sf sg yo zi sg zi aq aq md rj ln fe ed ro ed bt bt v bo v bo fe ro lj u gr gr dg lj dg ln u uq uq vn rp vn je je rp at ph nc b b kn tf kn tf ok mn mn nc ok sh yn yn yb zr dh zr dh o sc sc wq o wq sd yb sd ph ls ls sh ho lc mj mj lc e e nj d ye ye d hg dj nj mr mr ho bb bb vr mm vr mm pg pg at dj cc cc jg jg eo qh ql ql qh tm eo jd ti tm ti jd hg l jl pc pc ss ss km jl fk kq fk kq km nr vm vm vb gq vb gq l pp or or pp rj nr xd xf kh se vf se vf xf xd md kh xe zk fl bl pm pm ri ym tk tk hb gl yd ue br br ue il yd gl il dq dq hi hi jm zk jm af bg bg hb ym xr ri qe nq qe nq bl ii ii xr fl af zq xm ad ad yl xc pf gm gm xc pf zq oh oo oo yh lq yh lq ul dp dp xl dr g g dr yr yr zl zl sq rl sq rl ul xl wd ds ds oh wd rq yl tb tb vs vs fh q q wj fi fi lr lr fh i ej ej i wj jn jn xm dm dm rq rd ns rd n zh zh n rr dk dk og qf qf vl ob wi wi ob vl lh lh zc zc og rr hj hd hd c c up up ns ip gt gt p p ip hj ne ug ug mg nd ne cr qp cr hh hh qp nf r gg r nd mg bd nk nk gg nf bd xb te kd lm xb nh lm bq kd bq te nh ci sr wp oi th oi th no no ck sr ib oq oq ig np ir ni ni mb mb ir np pd lk pd lk ib yi yi ig ck us us wp qb fo qb ci fo ch if yq df vg cf ur ur im sj cf sj ui ui im ch vg qi zb wm eh wm zb eh df yq fn fn if qi vi ms ms go go vi yf mq mq po jq jq po yf ae jp fb jp pe jo jo pe dt hp ai ai h uh uh h ak pb pb ak pk pk dt un un pi gh gh pi hp ar ar pn nl ws ws cd rc rc cd nl pn fb cp cp rn rn ae mh rb rb oc mh oc os os wl jr jr wl on sb um um sb rf rf on td kk aj aj kl kl rk co co kk rk qo ic ic qo fr fr td bk gk ie ie bk gk qg rg rg qg f f")
  }

  test("d") {
    ProblemE("c r h r c h i d i d l l f q g e q f n e n g j j o o k k")
  }
}

