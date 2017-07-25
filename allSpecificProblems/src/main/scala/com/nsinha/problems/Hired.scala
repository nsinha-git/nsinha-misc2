package com.nsinha.problems

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 6/21/17.
  */
class Hired {

}

class Node(c : Char) {
  val kidsName = mutable.TreeSet[Char]()

  var leaf : Boolean = false

  val kidsMap = mutable.TreeMap[Char, Node]()

  def findTheNode(s : String) : Option[Node] = {
    if (s.isEmpty) return Option(this)
    val head = s(0)
    val tail = s.substring(1)

    if (kidsName.contains(head)) {
      kidsMap(head).findTheNode(tail)
    }
    else {
      None
    }
  }

  def returnAllLeaves(prefixPath : String = "") : List[String] = {
    val res = mutable.MutableList[String] ()

    val newPrefix = prefixPath + c
    if (leaf) res += newPrefix

    kidsMap foreach {
      case (k, node) ⇒
        node.returnAllLeaves(newPrefix) foreach { el ⇒
          res += el
        }
    }
    res.toList
  }

  def insert(s : String) : Boolean = {
    if (s.isEmpty) {
      leaf = true
      return true
    }

    val head = s(0)
    val tail = s.substring(1)

    if (kidsName.contains(head)) {
      val nextNode = kidsMap(head)
      nextNode.insert(tail)
    }
    else {
      kidsName += head
      val newNode = new Node(head)
      kidsMap += head → newNode
      newNode.insert(tail)
    }
  }
}

object PrefixTree {

  private val root = new Node('$')

  def insertWord(s : String) : Boolean = {
    root.insert(s)
  }

  def lookUpWordsFromTop(s : String) : List[String] = {
    val nodeOfInterestOpt = root.findTheNode(s)
    nodeOfInterestOpt match {
      case Some(nodeOfInt) ⇒ nodeOfInt.returnAllLeaves(s.substring(0, s.length - 1))
      case None            ⇒ Nil
    }
  }

  /*def lookupWords(s: String): List[String] = {
    val nodeOfInterestOpt = root.findTheNode(s)
    nodeOfInterestOpt match {
      case Some(nodeOfInt) => nodeOfInt.returnAllLeaves(s)
      case None => Nil
    }
  }*/

  def printAll = {

  }

}

class LoadDictionary(f : String = "/usr/share/dict/words") {

  def loadIntoPrefixTree = {

    val file = scala.io.Source.fromFile(f)

    val allLines = file.getLines()

    allLines foreach { word ⇒ PrefixTree.insertWord(word) }

    val list = PrefixTree.lookUpWordsFromTop("apple")
    println(list)

  }

}

class TestThisCode extends FunSuite {

  test ("1") {

    PrefixTree.insertWord("abc")

    PrefixTree.insertWord("abe")
    PrefixTree.insertWord("abef")

    PrefixTree.insertWord("dbc")

    val list = PrefixTree.lookUpWordsFromTop("a")
    println(list)

  }

  test ("2") {
    val n = new LoadDictionary()

    n.loadIntoPrefixTree

  }

}
