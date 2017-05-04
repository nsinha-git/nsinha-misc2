package com.nsinha.problems.BTree

import scala.collection.mutable

/** Created by nsinha on 4/25/17.
  */
case class SpecialInt(value : Option[Int] = None) extends Ordering[SpecialInt] {
  override def compare(x : SpecialInt, y : SpecialInt) = {
    (x.value, y.value) match {
      case (None, None)         ⇒ 0
      case (None, Some(z))      ⇒ 1
      case (Some(z), None)      ⇒ -1
      case (Some(z1), Some(z2)) ⇒ implicitly[Ordering[Int]].compare(z1, z2)
    }
  }
}

trait NodeTrait {
  val t : Int
  def searchRange(k : Int) : (Option[KeyNodeTrait], Option[KeyNodeTrait])
  def createBottom(v : String) : BottomNode
}

trait KeyNodeTrait extends NodeTrait {
  var keyArray = Array[(Int, KeyNodeTrait)]()

  private def binarySearchRange(start : Int, end : Int, k : Int) : (Option[(Int, KeyNodeTrait)], Option[(Int, KeyNodeTrait)]) = {
    //A1.entering the routine means the key must be less than the highest key in this node
    val initDist = end - start
    val left = keyArray(start)
    val right = keyArray(end)
    // following 3 bases cases are consistent with A1
    if (initDist == 0 && left._1 <= k) return (Option(left), None) //B1abeyond right most key
    if (initDist == 0 && left._1 >= k) return (None, Option(left)) //B1bbefore leftmost key
    if (initDist == 1) return (Option(left), Option(right)) //B2 beteween two keys
    // C1. middle !=left and middle !=right as initDistance >=2
    val middleIndx = math.min(((start + end) / 2), end)
    val middle = keyArray(middleIndx)

    if (left._1 >= k) return binarySearchRange(start, middle._1, k) // will recurse  to B1b

    if (middle._1 >= k) return binarySearchRange(start, middle._1, k)

    binarySearchRange(middle._1, end, k)
  }

  override def searchRange(k : Int) : (Option[(Int, KeyNodeTrait)], Option[(Int, KeyNodeTrait)]) = {
    if (keyArray.length == 0) (None, None)
    binarySearchRange(0, keyArray.length - 1, k)
  }
}

case class BottomNode(value : String, t : Int) extends KeyNodeTrait {
  override def searchRange(k : Int) = ???

  override def createBottom(v : String) = ???

}

case class LeafNode(t : Int) extends KeyNodeTrait {
  override def createBottom(v : String) = BottomNode(v, t)

  def insert(k : Int, value : String) = {
    val rangeSearchRes = searchRange(k)
    rangeSearchRes match {
      case (None, None) ⇒ //empty leaf
        insertBeforeAtKey(k, value, -1)
      case (Some(left), None) ⇒ //end
        insertAfterAtKey(k, value, left._1)
      case (None, Some(right)) ⇒
        insertBeforeAtKey(k, value, right._1)
      case (Some(left), Some(right)) ⇒
        insertBeforeAtKey(k, value, right._1)
    }
  }

  private def insertBeforeAtKey(k : Int, value : String, keyToBeInsertedBefore : Int) = {
    var tempKeyArray = Array[(Int, KeyNodeTrait)]()
    //copy (-1, Insertkey ) followed by new k and then [insertKey,N). totl size increases by 1
    keyArray.filter (x ⇒ x._1 < keyToBeInsertedBefore) map { tempKeyArray :+ _ }
    tempKeyArray :+ (k, createBottom(value))
    keyArray.filter (x ⇒ x._1 >= keyToBeInsertedBefore) map { tempKeyArray :+ _ }
    keyArray = tempKeyArray
  }

  private def insertAfterAtKey(k : Int, value : String, keyToBeInsertedAfter : Int) = {
    var tempKeyArray = Array[(Int, KeyNodeTrait)]()
    //copy (-1, Insertkey ] followed by new k and then (insertKey,N). totl size increases by 1
    keyArray.filter (x ⇒ x._1 <= keyToBeInsertedAfter) map { tempKeyArray :+ _ }
    tempKeyArray :+ (k, createBottom(value))
    keyArray.filter (x ⇒ x._1 > keyToBeInsertedAfter) map { tempKeyArray :+ _ }
    keyArray = tempKeyArray
  }

}

case class IntermediateTypeNode(t : Int) extends KeyNodeTrait {
  override def createBottom(v : String) = ???

  def insert(k : Int, value : String) : Boolean = {
    //higher logic has decided this is correct node but does not know if
    //in the intermediate node one should try inserting in lower or higher key depending on balance.
    // in case
    null
  }

}

sealed trait NodeType
case object BottomType extends NodeType
case object LeafType extends NodeType // a left type behaves as if its IntermediateType
case object RootType extends NodeType
case object IntermediateType extends NodeType
case object RootLeafType extends NodeType

class ResultType
case class ExactBottomKeyFound(node : BottomNode) extends ResultType
case object SizeExceeded extends ResultType
case class NearestKeysFound(leftNodeOpt : Option[KeyNodeTrait], rightOpt : Option[KeyNodeTrait]) extends ResultType
case object EmptyNode extends ResultType

trait BTree {
  def addKeyValue(k : Int, value : String)
  def removeKey(k : Int)
  def getRoot : NodeType
}

object BTree {
  def createBTree(t : Int = 5) : BTree = {
    new BTreeImpl(t)
  }

}

class BTreeImpl(t : Int) extends BTree {
  var root = new LeafNode(t) {}

  override def addKeyValue(k : Int, value : String) = {
    addKeyValueToNode(k, value, root)
  }

  def addKeyValueToNode(k : Int, value : String, node : KeyNodeTrait) = {

    val rangeSearchRes = node.searchRange(k)
    if (node.isInstanceOf[LeafNode]) {
      rangeSearchRes match {
        case (None, None)              ⇒ //empty leaf

        case (Some(left), None)        ⇒
        case (None, Some(right))       ⇒
        case (Some(left), Some(right)) ⇒

      }

    }
    else {

    }

  }

  override def removeKey(k : Int) = ???
  override def getRoot = ???
}

