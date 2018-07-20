package com.nsinha.problems.GoogleJam.Fourteen.WorldFinal.ProblemC

import org.scalatest.FunSuite

import scala.collection.mutable

/**
  * Created by nishchaysinha on 9/30/17.
  */
case class ProblemC(input:(Seq[Char], Seq[(Int,Int)])) {
  val size = input._1.size
  val nodeColMap = createNodeColHashMap
  val edges: Array[mutable.HashSet[Int]] = createEdgesHashMap
  val colsMapPerNode  = createColorGroups

  println(nodeColMap)
  println(edges)
  println(colsMapPerNode)
  println(isSymmetric)


  def createNodeColHashMap = {
    val mp = new Array[Char] (size)
    input._1 zip Range(0, size) foreach { case (col, indx) => mp(indx) = col }
    mp
  }

  def createEdgesHashMap = {
    val mp = new Array[mutable.HashSet[Int]] (size)
    for (i <- Range(0, size)){
      mp(i) = mutable.HashSet[Int]()
    }

    input._2 zip Range(0, size) foreach  {case (e,indx) =>
      val (n1,n2) = e
      mp(n1-1) += n2-1
      mp(n2-1) += n1-1
    }
    mp
  }

  def createColorGroups = {
    val mp = new Array[mutable.HashMap[Char,mutable.Set[Int]]](size)
    for (i <- Range(0, size)){
      mp(i) = mutable.HashMap[Char, mutable.Set[Int]]()
    }

    for (tuple <- input._2)  {
      val (n1,n2) = tuple
      val n1p = n1 -1
      val n2p = n2 -1
      val mapForn1 = mp(n1p)
      val mapForn2 = mp(n2p)

      if (!mapForn1.contains(nodeColMap(n2p))) { mapForn1  += (nodeColMap(n2p) -> mutable.HashSet[Int]()) }
      if (!mapForn2.contains(nodeColMap(n1p))) { mapForn2  += (nodeColMap(n1p) -> mutable.HashSet[Int]()) }
      mapForn1(nodeColMap(n2p)) += n2p
      mapForn2(nodeColMap(n1p)) += n1p

    }
    mp
  }


  def isSymmetric: Boolean = {

    /*
    create two queues with the nodes. may be sorted on colors.
    0.read first el from q1 and first el from q2. check if they can be reflexive.
    1. reflexive condition exists if the two nodes are of same color and have same color distribution on children.

    3 0 if yes: remove el1 from q1 and el2 from q2.  redo 0 with new queues.
    4 0 if no: try next el from q2. and goto 0. If no els on q2 then fail
     */


    val q1 = {
      val mutQ = mutable.MutableList[Int]()
      Range(0, size) foreach(mutQ += _)
      mutQ
    }
    val q2 = {
      val mutQ = mutable.MutableList[Int]()
      Range(0, size) foreach(x => mutQ += (x))
      mutQ
    }

    val axisPlacements = {
      Range(0, size).toList  map (_->3) toMap
    }

    runLoops(q1,q2, axisPlacements)
  }

  def runLoops (q1: mutable.MutableList[Int], q2: mutable.MutableList[Int], axisPlac: Map[Int, Int]): Boolean = {

    if (q1.isEmpty && q2.isEmpty) return true
    for (el1 <- q1) {
      for (el2 <- q2) {
        val res = isSte(el1, el2, axisPlac)
        if (res._1) {
          println(el1 + " " + el2)
          if (res._2 != None) {
            println(res._2.get)
            if (runLoops(q1 filter(x => x != el1 || x != el2), q2 filter(x => x != el1 || x != el2), res._2.get)) {
              return true
            }
          } else {
            if (runLoops(q1 filter(x => !(x == el1 || x == el2)), q2 filter(x => !(x == el1 || x == el2)), axisPlac)) {
              return true
            }
          }
        }
      }
      return false
    }
    return false

  }



  def isSte(el1: Int, el2: Int, axisPlac: Map[Int, Int]): (Boolean, Option[Map[Int, Int]]) = {
    if (nodeColMap(el1) != nodeColMap(el2)) return (false, None)

    val mp1 = colsMapPerNode(el1)
    val mp2 = colsMapPerNode(el2)

    if (mp1.size != mp2.size) return (false, None)
    for ( a <- mp1) {
      if (mp2(a._1).size != a._2.size) return (false, None)
    }

    if (el1 == el2) {
      val setOfNbrs = findNbrsOf(el1)
      setOfNbrs += el1
      var cond = true
      val newaxisPlac = axisPlac map { case (indx, value) =>
        if (setOfNbrs.contains(indx)) {
          if (value - 1 < 0) cond = false
          (indx, value - 1)
        } else {
          (indx, value)
        }
      }
      if (!cond ) {
        return (false, None)
      } else {
        return (true, Option(newaxisPlac))
      }
    }

    return (true, None)
  }

  def findNbrsOf(i: Int) = {
    edges(i)
  }


}



case class ProblemCTop(inStr: String) {
  val input:(Seq[Char], Seq[(Int,Int)]) = deriveInput
  println(input)
  new ProblemC(input)




  def deriveInput = {
    val splitsOnLine = inStr split("\n")
    val seqChars = mutable.MutableList[Char]()
    val edges = mutable.MutableList[(Int, Int)]()

    splitsOnLine foreach {x =>
      if (x.size == 1) {
        seqChars += x.toCharArray.head
      } else {
        val y = x.split(" ") map (_.toInt)
        edges  += ((y(0), y(1)))
      }
    }
    (seqChars.toList, edges.toList)
  }


}


class ProblemCTesting extends FunSuite {

  test("1") {
    ProblemCTop(
      """C
        |C
        |C
        |C
        |C
        |C
        |C
        |C
        |3 8
        |7 8
        |5 8
        |4 6
        |2 6
        |1 6
        |2 7""".stripMargin)

  }


}
