package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemE

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/29/17.
  */

case class SpanNode(left : Int, right : Int)

case class NodeAdjacency(fullyContained : mutable.Set[SpanNode] = mutable.HashSet(), partialIntersection : mutable.Set[SpanNode] = mutable.HashSet(), parents : mutable.Set[SpanNode] = mutable.HashSet())
case class ProblemE(input : String) {
  type Pos = Int

  val allSpanNode : List[SpanNode] = getSpanNodes

  val spanNodeGraph : mutable.Map[SpanNode, NodeAdjacency] = createEdges
  val spanNodeColor : mutable.Map[SpanNode, Int] = allSpanNode.foldLeft(mutable.HashMap[SpanNode, Int]()) { (Z, el) ⇒
    Z += el → 0
  }
  val dirsAvailable = Set("up", "down")

  //println({ HeightForGraph(spanNodeGraph, allSpanNode) }.solve)

  HyperGraph(spanNodeGraph.toMap, spanNodeColor)

  def getSpanNodes : List[SpanNode] = {
    val colors = input.split(" ")
    val colMap = mutable.Map[String, mutable.MutableList[Pos]] ()

    colors zip Range(0, colors.size) foreach { col ⇒
      if (!colMap.contains(col._1)) colMap += col._1 → mutable.MutableList[Pos]()
      colMap(col._1) += col._2
    }

    colMap map { x ⇒
      val a = x._2(0)
      val b = x._2(1)
      val left = if (a > b) b else a
      val right = if (a > b) a else b
      SpanNode(left, right)
    } toList
  }

  def createEdges : mutable.Map[SpanNode, NodeAdjacency] = {
    val mapOfNodes = mutable.HashMap[SpanNode, NodeAdjacency]()
    //do o(n^2)
    allSpanNode foreach {
      node ⇒
        mapOfNodes += node → NodeAdjacency()
    }

    for (curNode ← allSpanNode) {
      for (potNeighborNode ← allSpanNode if (potNeighborNode != curNode)) {
        if (isFullyContainedInLeftNode(curNode, potNeighborNode)) {
          mapOfNodes(curNode).fullyContained += potNeighborNode
        }
        else if (isPartialIntersection(curNode, potNeighborNode)) {
          mapOfNodes(curNode).partialIntersection += potNeighborNode
        }
        else if (isFullyContainedInLeftNode(potNeighborNode, curNode)) {
          mapOfNodes(curNode).parents += potNeighborNode
        }
        else {}
      }
    }
    mapOfNodes map { x ⇒ x._1 → x._2 }
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
//GT1 is graph of indpendent sets of span nodes.
//GT2 is graph of (span nodes, with dirs)

//eval(max-node(null, dirs=2) , eval(Gt2(dirs))
//eval(Gt1(nodes, dirs))      eval(Gt1(nodes, dirs)  ... eval(Gt1)
//eval[(max-node(dirsEva), eval(Gt2'(dirsEva)]  etc

class GT1(g : GraphSet) extends GraphBase(g) {

  def eval = {
    val (maxNode1, maxNode2) = findMaximalNodesOnConnectedGraph

  }

}
class GT2(g : GraphSet) extends GraphBase(g) {
  def eval = {
    val independentSets : List[GraphSet] = findIndependentSets

    independentSets foreach { independentSet ⇒
      val gt1 = new GT1(independentSet)
    }
  }
}

case class HyperGraph(g : Map[SpanNode, NodeAdjacency], gColor : mutable.Map[SpanNode, Int]) {

  case class ColoredNode(spanNode : SpanNode, var color : Int) {
    override def hashCode() : Height = spanNode.hashCode()
  }

  var failureOfBiPartite = false
  val heightDpMap = mutable.HashMap[SpanNode, Array[(Height, Height)]]()
  val coloredNodeBaseGraph : Map[ColoredNode, List[ColoredNode]] = createColoredGraph

  //should not do this
  //processColors

  processTheDfsOfHeight

  def createColoredGraph : Map[ColoredNode, List[ColoredNode]] = {
    val coloredGraph = mutable.HashMap[ColoredNode, List[ColoredNode]]()
    val spanNodeToColoredNode = mutable.HashMap[SpanNode, ColoredNode]()

    g.keys foreach { x ⇒
      val colNode = ColoredNode(x, 0)
      coloredGraph += colNode → List[ColoredNode]()
      spanNodeToColoredNode += x → colNode
    }
    g foreach { x ⇒
      val colNode : ColoredNode = spanNodeToColoredNode(x._1)

      //  val coloredNodeNbrs = x._2 map { spanNodeToColoredNode(_) }

      //coloredGraph += (colNode → coloredNodeNbrs)
    }

    coloredGraph.toMap
  }

  def processColors = { doBfsForColorSetting }

  def doBfsForColorSetting = {
    val allNodes = coloredNodeBaseGraph.keys.foldLeft(mutable.MutableList[ColoredNode]()) { (Z, el) ⇒ Z += el } toSet
    val allNonZeroNodes = { allNodes filter { x ⇒ coloredNodeBaseGraph(x).size > 0 } }.foldLeft(mutable.Set[ColoredNode]()) { (Z, el) ⇒ Z += el }

    val visited = mutable.Set[ColoredNode]()

    while (allNonZeroNodes.nonEmpty) {
      val curRoot = allNonZeroNodes.head
      allNonZeroNodes.remove(curRoot)
      curRoot.color = 1
      val bfsQue = mutable.Queue[ColoredNode](curRoot)
      doBfsForColorSettingOnQue(bfsQue, visited)
      allNonZeroNodes.intersect(visited) foreach { x ⇒ allNonZeroNodes -= (x) }
    }
  }

  def doBfsForColorSettingOnQue(bfsQue : mutable.Queue[ColoredNode], visited : mutable.Set[ColoredNode]) : Unit = {
    //bfs Que nodes have colored already set
    val curVisiting = bfsQue.dequeue()
    val curColor = curVisiting.color
    val oppColor = if (curColor == 1) 2 else 1

    val allNbrs = coloredNodeBaseGraph(curVisiting)

    val isFailure = allNbrs.foldLeft (false) { (Z, el) ⇒
      if (Z == true) {
        Z
      }
      else {
        val res = if (el.color == 0) {
          el.color = oppColor
          false
        }
        else {
          if (el.color != oppColor) { true } else { false }
        }
        if (res == true) true else Z
      }
    }

    if (isFailure) failureOfBiPartite = true
    visited += curVisiting
    allNbrs foreach { x ⇒ if (!visited.contains(x)) { bfsQue += x } }

    if (bfsQue.isEmpty) return else doBfsForColorSettingOnQue(bfsQue, visited)
  }

  type Height = Int

  var maxHeight : Height = 0
  var failure = false

  def processTheDfsOfHeight : Height = {
    // the nodes in coloredNodeBaseGraph can form a forest. so we need to be able to subtract after we visit each of trees.
    // we can create a mutable set of all nodes to start with. We will subtract a node from this set as we finish off with that node.
    //we will keep prcoessing ad updating Height till all of set is exhausted.

    val nodesQueUnvisited = g.foldLeft(mutable.Set[SpanNode]()) { (Z, el) ⇒
      Z += el._1
    }

    while (nodesQueUnvisited.nonEmpty) {
      val curNode = findFarthestParentNode(nodesQueUnvisited.head) //prove that a unvisited node farthest parent would be always unvisited in dfs
      nodesQueUnvisited.remove(curNode)
      doDfsHeight(curNode, nodesQueUnvisited)
    }

    println(maxHeight)
    maxHeight
  }

  def findFarthestParentNode(node : SpanNode) : SpanNode = { if (g(node).parents.isEmpty) node else findFarthestParentNode(g(node).parents.head) }

  def doDfsHeightTop(curNode : SpanNode, unVisited : mutable.Set[SpanNode]) : Unit = {
    if (heightDpMap.contains(curNode)) return
    val parentNode = findFarthestParentNode(curNode)
    unVisited.remove(parentNode)
    doDfsHeight(parentNode, unVisited)
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
        assert(false) //we should never be here curColor is for aggregating grandestParentNode and this cant be reacges
      }
      else { //color is 2
        curNodeIntersection foreach (x ⇒ if (gColor(x) == 0) gColor(x) = 1 else if (gColor(x) == 1) assert(true) else assert(false))
      }
    }
    val color = gColor(curNode)
    val curNodeChildren = g(curNode).fullyContained
    val curNodeChildrenUnvisited = curNodeChildren filter { x ⇒ unVisited.contains(x) }

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
      findBestHeightsFromChildren(childHeightsTop, color)
    }
    else {
      //there must be children who remain unvisited. unconditionally visit everyone from here
      curNodeChildrenUnvisited foreach (doDfsHeightTop(_, unVisited))
      val childHeightsTop = curNodeChildren map { el ⇒ heightDpMap(el) }
      findBestHeightsFromChildren(childHeightsTop, color)
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
          val heightArrayFromIntesectingGrandParent = heightDpMap(grandestParentOfIntersectionSet.head)
          // we have two arrays of height options for this node. one for its direct kids and other for its intersection.
          //the heights are already constarined in two separate direcctions
          //we just expect the array of each to contain a single entry
          assert(heightArrayFromIntesectingGrandParent.size == 1)
          assert(heightArrayFromFullyContainedChildren.size == 1)
          //for this node choose the highest axis from two elements taken one at a time from each array
          val maxX = Math.max(heightArrayFromIntesectingGrandParent.head._1, heightArrayFromFullyContainedChildren.head._1)
          val maxY = Math.max(heightArrayFromIntesectingGrandParent.head._2, heightArrayFromFullyContainedChildren.head._2)
          val newHeightTuple = Array((maxX, maxY))
          heightDpMap += curNode → newHeightTuple
          newHeightTuple foreach { el ⇒
            if (el._1 > maxHeight) maxHeight = el._1
            if (el._2 > maxHeight) maxHeight = el._2
          }

          newHeightTuple
        }
      }
      heightArrayFromIntersects
    }
    else {
      heightDpMap += curNode → heightArrayFromFullyContainedChildren
      heightArrayFromFullyContainedChildren foreach { el ⇒
        if (el._1 > maxHeight) maxHeight = el._1
        if (el._2 > maxHeight) maxHeight = el._2
      }
      heightArrayFromFullyContainedChildren
    }
  }

  def findBestHeightsFromChildren(childrenHeights : mutable.Set[Array[(Height, Height)]], color : Int) : Array[(Height, Height)] = {
    val minXLowerBound = findMaxOfMinOnListOfTuples(childrenHeights, 0)
    val minYLowerBound = findMaxOfMinOnListOfTuples(childrenHeights, 1)

    val childHeightsTopConditionedOnX = filterArrayBasedOnAxis(childrenHeights, 0, minXLowerBound)
    val childHeightsTopConditionedOnY = filterArrayBasedOnAxis(childrenHeights, 1, minYLowerBound)

    val maxYForMinXBound = {
      childHeightsTopConditionedOnX.map(_._2)
    }.max
    val maxXForMinYBound = {
      childHeightsTopConditionedOnY.map(_._1)
    }.max

    //(minX, maxYCOn) and (maxXCond, minY) are two options
    val xGroupTuple = (minXLowerBound, maxYForMinXBound)
    val yGroupTuple = (minYLowerBound, maxXForMinYBound)
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

    val resArrayUnfiltered = color match {
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
      case 1 ⇒ preResTupleArray flatMap { preResTuple ⇒ Array((preResTuple._1 + 1, preResTuple._2)) }
      case 2 ⇒ preResTupleArray flatMap { preResTuple ⇒ Array((preResTuple._1, preResTuple._2 + 1)) }
      case _ ⇒ throw new RuntimeException()
    }

    val minHeight = { preResTupleArray map (x ⇒ Math.max(x._1, x._2)) }.min

    preResTupleArray filter { x ⇒ Math.max(x._1, x._2) < minHeight }
  }

  private def filterArrayBasedOnAxis(ll : mutable.Set[Array[(Height, Height)]], axis : Int, bound : Height) : List[(Height, Height)] = {
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

  private def findMaxOfMinOnListOfTuples(ll : mutable.Set[Array[(Height, Height)]], axis : Int) : Height = {
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
}

