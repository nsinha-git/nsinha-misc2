package com.nsinha.problems.RedBlackTree

import com.nsinha.common.CircularQueue
import com.nsinha.common.StandardBooleanCoercions._
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 4/21/17.
  */
trait RedBlackTreeSet {
  def addElem(x : Int)
  def isPresent(x : Int) : Boolean
  var root : Node
}

class Node {
  var left : Node = _
  var right : Node = _
  var value : Int = 0
  var height : Int = 0
  var color : Int = 0

  override def toString : String = {
    val l = if (left) left.value.toString else "∅"
    val r = if (right) right.value.toString else "∅"
    val curVal = value.toString
    curVal + s"($height)-"+l+"-"+r
  }
}

//hotspot is a child of node which we have reached just now. The reached node may be unbalanced and needs to be fixed.
//Fixing is always done as a function on 5 nodes. The curNode, it's left child, it's right child. One of the child is hot spot: so we add
// the hot'spots children making it curNode, left chi, right child, leftHotSpotkid, rightHotSpotKid.

trait HotSpot {
  def getRoot : Node
}

object RedBlackTreeSet {
  def createSet(initialEl : Int) = new RedBlackImpl(initialEl)
}

class RedBlackImpl(initRoot : Int, multiplier : Int = 2, offset : Int = 1) extends RedBlackTreeSet {

  override var root = new Node { value = initRoot }

  override def isPresent(x : Int) : Boolean = isPresent(x, root)

  private def isPresent(newEntry : Int, node : Node) : Boolean = {
    if (node == null) return false
    if (newEntry == node.value) return true
    if (newEntry < node.value) { isPresent(newEntry, node.left) } else { isPresent(newEntry, node.right) }
  }

  override def addElem(newEntry : Int) = {
    val circularQueueOfParents = new CircularQueue[Node](100)
    val nodeInHotSpotOpt = insertElem(newEntry, root, circularQueueOfParents)
    nodeInHotSpotOpt match {
      case Some(nodeInHotSpot) ⇒
        val parentOpt = circularQueueOfParents.get
        val gParentOpt = circularQueueOfParents.get
        val gGparentOpt = circularQueueOfParents.get
        val hotSpot = createHotSpotFromNode(nodeInHotSpot)
        takeCareOfHotSpot(hotSpot, parentOpt, gParentOpt, gGparentOpt, circularQueueOfParents)
        printBfsTree
      case None ⇒
    }
  }

  def insertElem(newElem : Int, node : Node, que : CircularQueue[Node]) : Option[Node] = {
    if (node.value == newElem) return None
    que.insert(node)
    if (node.value < newElem) {
      if (node.right) insertElem(newElem, node.right, que) else {
        val newNode = new Node { value = newElem }
        node.right = newNode
        node.height = if (node.left) { math.max(node.left.height, node.right.height) + 1 } else { 1 }
        que.insert(newNode)
        que.get
      }
    }
    else {
      if (node.left) insertElem(newElem, node.left, que) else {
        val newNode = new Node { value = newElem }
        node.left = newNode
        node.height = if (node.right) { math.max(node.right.height, node.left.height) + 1 } else { 1 }
        que.insert(newNode)
        que.get
      }
    }
  }

  def createHotSpotFromNode(n : Node) : HotSpot = {
    new HotSpot {
      override def getRoot = n
    }
  }

  private def switchKid(gGParentOpt : Option[Node], curKid : Node, toSwitchKid : Node) : Unit = {
    Option(gGParentOpt) map { x ⇒
      x match {
        case Some(gGParent) ⇒
          if (gGParent.left && gGParent.left == curKid) {
            gGParent.left = toSwitchKid
            return
          }
          if (gGParent.right && gGParent.right == curKid) {
            gGParent.right = toSwitchKid
            return
          }
        case None ⇒ //gGParentOpt is not even there ie gParent is root
          root = toSwitchKid
      }
    }
  }

  def takeCareOfHotSpot(hotSpot : HotSpot, parentOpt : Option[Node], gParentOpt : Option[Node], gGParentOpt : Option[Node], parentsQue : CircularQueue[Node]) : Option[HotSpot] = {
    println(s"HotSpot balance new iteration")
    printBfsTree
    (parentOpt, gParentOpt, gGParentOpt) match {
      case (Some(parent), None, None) ⇒
        val isLeftChildHotSpot = if (parent.left && parent.left == hotSpot.getRoot) true else false
        val leftHeightParent = if (parent.left) (parent.left.height + 1) else 0
        val rightHeightParent = if (parent.right) parent.right.height + 1 else 0
        val lesserHeightOfParent = math.min(leftHeightParent, rightHeightParent)
        val greaterHeightOfParent = math.max(leftHeightParent, rightHeightParent)
        parent.height = math.max(leftHeightParent, rightHeightParent)

        if (multiplier * lesserHeightOfParent + offset <= greaterHeightOfParent) {
          val n = hotSpot.getRoot
          val leftN = n.left
          val rightN = n.right
          switchKid(None, parent, n)
          if (isLeftChildHotSpot) {
            n.right = parent
            parent.left = rightN
          }
          else {
            n.left = parent
            parent.right = leftN
          }
          recalculateHeight(parent)
          recalculateHeight(n)
        }

      case (Some(parent), Some(gParent), _) ⇒ //general case
        //current node is Gparent. yeah we fix the grandparent in each step.Because
        // the first parent is always balanced by default in red black tree.
        //case 1: first child of parent: done
        //case 2: second child of parent: done

        val isLeftChildHotSpot = if (parent.left && parent.left == hotSpot.getRoot) true else false
        val isLeftParentGrandParent = if (gParent.left && gParent.left == parent) true else false
        val leftHeightParent = if (parent.left) parent.left.height + 1 else 0
        val rightHeightParent = if (parent.right) parent.right.height + 1 else 0
        parent.height = math.max(leftHeightParent, rightHeightParent)
        //fix the grandParentHeight

        val leftHeightGrandParent = if (gParent.left) gParent.left.height + 1 else 0
        val rightHeightGrandParent = if (gParent.right) gParent.right.height + 1 else 0
        gParent.height = math.max(leftHeightGrandParent, rightHeightGrandParent)
        val lesserHeightOfGrandParent = math.min(leftHeightGrandParent, rightHeightGrandParent)
        val greaterHeightOfGrandParent = math.max(leftHeightGrandParent, rightHeightGrandParent)

        if (multiplier * lesserHeightOfGrandParent + offset <= greaterHeightOfGrandParent) {
          //Gparent needs fixing
          if (isLeftParentGrandParent) {
            //lesser height of GrandParnet is coming from its right child
            if (isLeftChildHotSpot && rightHeightGrandParent == rightHeightParent) {
              //ly exists and is on left
              if (rightHeightParent) {
                val rightKidOfParentLz = parent.right
                switchKid(gGParentOpt, gParent, parent)
                parent.right = gParent
                gParent.left = rightKidOfParentLz
                recalculateHeight(hotSpot.getRoot)
                recalculateHeight(gParent)
                recalculateHeight(parent)
                val newHotSpot = new HotSpot { override def getRoot = parent }
                takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
              }
              else {
                //lz =0 lz does not exist
                val rightKidOfParentLz = null
                if (!rightHeightGrandParent) {
                  //only happens when HP is single node
                  switchKid(gGParentOpt, gParent, parent)
                  assert(hotSpot.getRoot.left == null)
                  assert(hotSpot.getRoot.right == null)
                  gParent.left = null
                  parent.right = gParent
                  recalculateHeight(hotSpot.getRoot)
                  recalculateHeight(gParent)
                  recalculateHeight(parent)
                  val newHotSpot = new HotSpot { override def getRoot = parent }
                  takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
                }
              }
            }
            else if (!isLeftChildHotSpot && rightHeightGrandParent == leftHeightParent) {
              //ly exists and is on right
              if (leftHeightParent) {
                //lz
                val rightKidOfParentLy = parent.right
                switchKid(gGParentOpt, gParent, parent)
                parent.right = gParent
                gParent.left = rightKidOfParentLy
                recalculateHeight(hotSpot.getRoot)
                recalculateHeight(gParent)
                recalculateHeight(parent)
                val newHotSpot = new HotSpot { override def getRoot = parent }
                takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
              }
              else {
                //lz =0 lz does not exist
                val leftKidOfParentLz = null
                if (!rightHeightGrandParent) {
                  //only happens when HP is single node
                  switchKid(gGParentOpt, gParent, hotSpot.getRoot)
                  assert(hotSpot.getRoot.left == null)
                  assert(hotSpot.getRoot.right == null)
                  hotSpot.getRoot.left = parent
                  hotSpot.getRoot.right = gParent
                  gParent.left = null
                  parent.right = null
                  recalculateHeight(gParent)
                  recalculateHeight(parent)
                  recalculateHeight(hotSpot.getRoot)
                  val newHotSpot = new HotSpot { override def getRoot = hotSpot.getRoot }
                  takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
                }
              }
            }
            else {
              //use A4.
              //this should never have happened as rightHeightGrandParent is larger than minimum height of parent.
              //as parent is balanced by induction the rightHeightGrandParent must be good enough to balance the
              //hotspot
              assert(false)
            }
          }
          else {
            // else clause of if(isLeftParentGrandParent)
            if (!isLeftChildHotSpot && leftHeightGrandParent == leftHeightParent) {
              //ly exists and is on right
              if (leftHeightParent) {
                //lz
                val leftKidOfParentLz = parent.left
                switchKid(gGParentOpt, gParent, parent)
                parent.left = gParent
                gParent.right = leftKidOfParentLz
                recalculateHeight(hotSpot.getRoot)
                recalculateHeight(gParent)
                recalculateHeight(parent)
                val newHotSpot = new HotSpot { override def getRoot = parent }
                takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
              }
              else {
                //lz =0 lz does not exist
                val leftKidOfParentLz = null
                if (!leftHeightGrandParent) {
                  //only happens when HP is single node
                  switchKid(gGParentOpt, gParent, parent)
                  assert(hotSpot.getRoot.left == null)
                  assert(hotSpot.getRoot.right == null)
                  gParent.right = null
                  parent.left = gParent
                  recalculateHeight(hotSpot.getRoot)
                  recalculateHeight(gParent)
                  recalculateHeight(parent)
                  val newHotSpot = new HotSpot { override def getRoot = parent }
                  takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
                }
              }
            }
            else if (isLeftChildHotSpot && leftHeightGrandParent == rightHeightParent) {
              //ly exists and is on left
              if (rightHeightParent) {
                //lz
                val leftKidOfParentLy = hotSpot.getRoot
                switchKid(gGParentOpt, gParent, parent)
                parent.left = gParent
                gParent.right = leftKidOfParentLy
                recalculateHeight(hotSpot.getRoot)
                recalculateHeight(gParent)
                recalculateHeight(parent)
                val newHotSpot = new HotSpot { override def getRoot = parent }
                takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
              }
              else {
                //lz =0 lz does not exist
                val leftKidOfParentLz = null
                if (!leftHeightGrandParent) {
                  //only happens when HP is single node
                  switchKid(gGParentOpt, gParent, hotSpot.getRoot)
                  assert(hotSpot.getRoot.left == null)
                  assert(hotSpot.getRoot.right == null)
                  hotSpot.getRoot.left = parent
                  hotSpot.getRoot.right = gParent
                  gParent.left = null
                  parent.right = null
                  recalculateHeight(gParent)
                  recalculateHeight(parent)
                  recalculateHeight(hotSpot.getRoot)
                  val newHotSpot = new HotSpot { override def getRoot = hotSpot.getRoot }
                  takeCareOfHotSpot(newHotSpot, gGParentOpt, parentsQue.get, parentsQue.get, parentsQue)
                }
              }
            }
            else {
              //use A4.
              //this should never have happened as rightHeightGrandParent is larger than minimum height of parent.
              //as parent is balanced by induction the rightHeightGrandParent must be good enough to balance the
              //hotspot
              assert(false)
            }
          }

        }
        else {
          val newHotSpot = new HotSpot { override def getRoot = parent }
          takeCareOfHotSpot(newHotSpot, gParentOpt, gGParentOpt, parentsQue.get, parentsQue)

        }

      case _ ⇒ //ignore as per A3.
    }

    None

  }

  def recalculateHeight(node : Node) : Unit = {
    node && {
      val htLeft = if (node.left) node.left.height else -1
      val htRight = if (node.right) node.right.height else -1
      node.height = math.max(htLeft, htRight) + 1
      true
    }
  }

  def printBfsTree = {
    println("-------")
    bfsVisitLevelWiseAndPrint(mutable.Queue(root))
    println("-------")
  }

  def bfsVisitLevelWiseAndPrint(que : mutable.Queue[Node]) = {
    val futureQue = mutable.Queue[Node]()
    while (que.nonEmpty) {
      while (que.nonEmpty) {
        val curNode = que.dequeue()
        print(curNode+"  ")
        curNode.left && { futureQue += curNode.left }
        curNode.right && { futureQue += curNode.right }
      }
      println()
      while (futureQue.nonEmpty) {
        val curEl = futureQue.dequeue()
        que += curEl
      }
    }

  }

}

class RedBlackImplTesting extends FunSuite {

  test("a") {
    val tree : RedBlackTreeSet = RedBlackTreeSet.createSet(0)
    tree.addElem(1)
    tree.addElem(2)
    tree.addElem(3)
    tree.addElem(4)
    tree.addElem(5)
    tree.addElem(6)
  }

  test("b") {
    val tree : RedBlackTreeSet = RedBlackTreeSet.createSet(0)
    for (i ← Range(1, 20)) tree.addElem(i)
  }

  test("c") {
    val tree : RedBlackTreeSet = RedBlackTreeSet.createSet(4)
    tree.addElem(3)
    tree.addElem(2)
    tree.addElem(1)
    tree.addElem(0)
    tree.addElem(-1)
    tree.addElem(-2)
  }

  test("d") {
    val tree : RedBlackTreeSet = RedBlackTreeSet.createSet(0)
    for (i ← Range(1, 20)) {
      tree.addElem(-i)
      tree.addElem(i)
    }
  }

}
