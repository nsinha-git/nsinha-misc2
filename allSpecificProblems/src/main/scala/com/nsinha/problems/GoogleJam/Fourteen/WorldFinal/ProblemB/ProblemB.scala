package com.nsinha.problems.GoogleJam.Fourteen.WorldFinal.ProblemB

import org.scalatest.FunSuite

import scala.collection.immutable.TreeSet
import scala.collection.mutable
import com.nsinha.common.PermutationCombination._

/** Created by nsinha on 6/2/17.
  */

case class DpKey(l : Array[Int], moves : List[Int]) {
  override def toString = {
    val x = l.foldLeft("nos") { (Z, el) ⇒ Z+"_"+el }
    moves.sorted.foldLeft(x+"moves"){
      (Z, el) ⇒ Z+"_"+el
    }
  }

}

object ProblemB {

  val dpTable : mutable.TreeMap[String, Long] = mutable.TreeMap[String, Long]()

  val dpMoves : mutable.HashSet[List[(Int, Int, Int)]] = mutable.HashSet()

  def getStringFromPerm(x : Array[Int]) : String = {
    x.foldLeft("") {
      (Z, el) ⇒ Z+"-"+el
    }
  }

  def noShared(s : Int, t : Int, size : Int) : Boolean = {
    val rangeSize = math.pow(2, size).toInt
    if (s == t) return false
    val diff = math.max(s, t) - math.min(s, t)
    if (diff >= rangeSize) true else false
  }

  def createACycle(pair : (Int, Int), map : mutable.Map[Int, Int]) : List[Int] = {
    val mList = mutable.MutableList[Int]()
    map - pair._1
    if (pair._1 == pair._2) {
      mList += pair._1
    }
    else {
      mList :+ pair._1
      mList :+ pair._2
      var mSet = mutable.Set[Int]()
      var cond = true
      var link = pair._2
      while (cond) {

      }

    }

    mList.toList
  }

  def getPermNormalizedRepresentation(l : Array[Int]) = {
    val totalNos = l.size
    val transMap = mutable.Map[Int, Int]()
    val fullCycles = mutable.Set[List[Int]]()
    val visited = mutable.Set[Int]()

    for (i ← Range(0, totalNos)) {
      val x = l(i)
      transMap += x → i
    }
    var cond = true

    while (cond) {
      val cur = transMap.headOption
      cur match {
        case None ⇒
          cond = false
        case Some(pair) ⇒
          createACycle(pair, transMap)
      }
    }

  }

}

case class ProblemB(l : Array[Int], n : Int, moves : List[Int] = List(), movesExact : List[(Int, Int, Int)] = Nil) {
  import ProblemB._
  val allMoves = Range(0, n).foldLeft(Set[Int]()) { (Z, el) ⇒ Z + el }
  val identity = Range(0, math.pow(2, n).toInt).toArray[Int]
  val identityString = getStringFromPerm(identity)

  def generateAllOffsets(size : Int) : List[Int] = {
    val rangeSize = math.pow(2, size).toInt
    val m = mutable.MutableList[Int]()
    for (pos ← identity) {
      if (pos % rangeSize == 0) m += pos
    }
    m.toList
  }

  def makeMove(startPos : Int, targetPos : Int, size : Int) : Array[Int] = {
    val rangeSize = math.pow(2, size).toInt
    val lMutable = l.clone()

    for (i ← Range(0, rangeSize)) {
      val temp = lMutable(i + startPos)
      lMutable(i + startPos) = lMutable(i + targetPos)
      lMutable(i + targetPos) = temp
    }
    lMutable
  }
  def solve = {
    solveFirstSet
    var sum = 0L

    for (el ← dpMoves) {
      sum = sum + factorial(el.size)
    }
    sum
  }

  def solveFirstSet : Long = {
    if (dpTable.contains(DpKey(l, moves).toString)) {
      return dpTable(DpKey(l, moves).toString)
    }
    var tot = 0l
    if (getStringFromPerm(l) == identityString) {
      return tot
    }
    var cond = true

    for (candMove ← allMoves.diff(moves.toSet)) {
      if ((moves.nonEmpty && moves.max < candMove) || moves.isEmpty) {
        val starts = generateAllOffsets(candMove)
        for {
          start1 ← starts if cond
          start2 ← starts if (noShared(start1, start2, candMove) && start2 > start1)
        } {
          val newPerm = makeMove(start1, start2, candMove)
          val newPermExactMoves = movesExact :+ (start1, start2, math.pow(2, candMove).toInt)
          if (getStringFromPerm(newPerm) == identityString) {
            dpMoves += newPermExactMoves
            println(newPermExactMoves)
            tot = tot + 1
          }
          else {
            val subTot = {
              ProblemB(newPerm, n, moves :+ candMove, newPermExactMoves) solveFirstSet
            }
            if (subTot > 0) {
              newPerm foreach {
                print(_)
              }
              print(":"+s"${moves :+ candMove map (math.pow(2, _).toInt)}:${start1} $start2: ${tot} ${subTot} ${tot + subTot}\n")
              tot = tot + subTot
            }
          }
        }
      }
      else {

      }
    }
    dpTable += (DpKey(l, moves).toString → tot)
    tot
  }
}

class TestingB extends FunSuite {
  test ("a") {

    val arr : Array[Int] = Array(9, 10, 13, 14, 11, 12, 15, 16, 5, 6, 7, 8, 1, 2, 3, 4)
    val pB = ProblemB(arr map (_ - 1), 4)

    val res = pB.solve
    print(res)
    println(ProblemB.dpMoves)

  }

  test ("b") {

    val arr : Array[Int] = Array(7, 8, 5, 6, 1, 2, 4, 3)
    val pB = ProblemB(arr map (_ - 1), 3)

    val res = pB.solve
    print(res)
    println(ProblemB.dpMoves)

  }

  test ("c") {

    val arr : Array[Int] = Array(7, 8, 5, 6, 1, 2, 4, 3)
    val pB = ProblemB(arr map (_ - 1), 3)

    val res = pB.solve
    print(res)
    println(ProblemB.dpMoves)

  }

  test ("d") {

    val arr : Array[Int] = Array(2, 1)
    val pB = ProblemB(arr map (_ - 1), 1)

    val res = pB.solve
    print(res)
    println(ProblemB.dpMoves)

  }

  test ("e") {

    val arr : Array[Int] = Array(7, 8, 27, 12, 13, 14, 15, 16, 25, 26, 11, 28, 29, 30, 31, 32, 1, 2, 3, 4, 5, 6, 9, 10, 17, 18, 19, 20, 21, 22, 23, 24)
    val pB = ProblemB(arr map (_ - 1), 5)

    val res = pB.solve
    print(res)
    println(ProblemB.dpMoves)

  }
}
