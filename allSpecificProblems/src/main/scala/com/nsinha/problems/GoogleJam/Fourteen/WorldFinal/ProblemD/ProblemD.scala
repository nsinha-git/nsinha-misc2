package com.nsinha.problems.GoogleJam.Fourteen.WorldFinal.ProblemD

import org.scalatest.FunSuite

import scala.collection.mutable

/**
  * Created by nishchaysinha on 10/1/17.
  */

/*
-Y
N-
 */
case class ProblemDTop(inStr: String, z: Int) {
  val edges = getEdges toMap
  val topoMap = {ProblemD(edges, z)}.doTopoOrdering
  val isComplete = topoMap.foldLeft(0) {(Z,e)=> Z + e._2.size} == edges.size
  if (isComplete) {
    val l = topoMap.foldLeft(mutable.MutableList[Int]()) { (Z, el) =>
      Z.++:(el._2.sorted.reverse)
    }
    l foreach(x => print(x + " "))
  } else {
    println("IMPOSS")
  }

  def getEdges = {
    val mp = mutable.Map[Int, Array[Char]]()
    val lines = inStr.split("\n")

    lines zip Range (0, lines.size) foreach {case (l, indx) =>
      val ll = l.toCharArray
      mp += indx -> ll
    }
    mp
  }

}
case class ProblemD(edges:Map[Int, Array[Char]] , zero: Int) {
  /*
  one of the int is at level 0.
  any direct nbr of 0 is at level 1
  any direct nbr of 1 is 2 etc

  When sequenceing just pick level by level


   */

  def findNbrsOf(indx: Int): List[Int] = {
    val array = edges(indx)
    val mL = mutable.MutableList[Int]()
    for (i <- Range(0, array.size)) {
      if (array(i) == 'Y') {
        mL += i
      }
    }
    mL.toList
  }
  def doTopoOrdering: mutable.SortedMap[Int, mutable.MutableList[Int]] = {

    val m = mutable.Queue[(Int, Int)]()
    val visited = mutable.Set[Int]()
    val hSet = mutable.HashMap[Int, Int]()
    m += zero -> 0
    visited += zero

    while (m.nonEmpty) {
      val el = m.dequeue()
      hSet += el
      val curKnownLevelNode = el._1
      val nextLevel = el._2 + 1
      val nodesNbr: List[Int] = findNbrsOf(curKnownLevelNode)
      for (elNbr <- nodesNbr if (!visited.contains(elNbr))) {
        m += elNbr -> nextLevel
        visited += elNbr
      }
    }

    //m contains the (int,int) we need to group on 2nd axis of tuple

    val grpBy = mutable.SortedMap[Int, mutable.MutableList[Int]]()

    for (el <- hSet) {
      if (grpBy.contains(el._2)) {
        grpBy(el._2) += el._1
      } else {
        grpBy += el._2 -> mutable.MutableList[Int]()
        grpBy(el._2) += el._1
      }
    }

    grpBy

  }

}


class ProblemDTesting extends FunSuite {

  test("1") {
    ProblemDTop(
      """-YNN
        |N-YY
        |YN-Y
        |YNN-""".stripMargin, 3)
  }

}



