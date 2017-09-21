package com.nsinha.problems.GoogleJam.Eleven.WorldFinal.ProblemD

import org.apache.lucene.util.packed.PackedInts.Mutable
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nishchaysinha on 7/18/17.
  */
case class ProblemD(n : Int) {

  def generatePerms() : List[List[Int]] = {
    generatePerms(Range(1, n + 1).toList, Nil)
  }

  def generatePerms(ints : List[Int], prevList : List[List[Int]]) : List[List[Int]] = {
    if (ints.isEmpty) {
      prevList
    }
    else {
      ints flatMap { x ⇒
        val z = if (prevList == Nil) {
          List(List(x))
        }
        else {
          prevList map { y ⇒
            y :+ x
          }
        }
        generatePerms(ints filter (_ != x), z)
      }
    }
  }

  def getValidPerms : List[List[Int]] = {
    generatePerms() filter (x ⇒ validate(x))
  }

  /*
  4321
  2-> 1
  3-> 1,2
  4 ->1,2,3

   */
  def validate(l : List[Int]) : Boolean = {
    val lRev = l.reverse
    val mp = mutable.Map[Int, (Int, mutable.MutableList[Int])]()

    for (tuple ← lRev zip Range(0, lRev.size)) {
      val (elAtRight, posR) = (tuple._1, lRev.size - tuple._2 - 1)
      for (posL ← Range(0, l.size) if posL < posR) {
        val elAtLeft = l(posL)
        if (elAtLeft > elAtRight) {
          mp.get(posL) match {
            case None ⇒
              mp += (posL → (elAtLeft, mutable.MutableList(posR)))
            case Some(mpVal) ⇒
              mpVal._2 += posR
          }
        }
      }
    }

    val mpPos = mp map { z ⇒ z._1 → z._2._2 }
    for { kv ← mpPos } {
      val (key, value) = kv
      for (nbr ← value) {

        if (mpPos.get(nbr) != None && mpPos(nbr).nonEmpty) {
          return false
        }
      }
    }
    true
  }

  def solve(trailPath : List[Int]) : List[Int] = {
    var all = getValidPerms
    val mutableList = mutable.MutableList[(Int, Int)]()
    val knownPos = trailPath(trailPath.size - 1) - 1
    for (curSelection ← trailPath map (x ⇒ x - 1)) {
      //println(trailPath)
      //println(s"curPos:$curSelection")
      val valueAtCurPos = findHighestVaritionWithAKnownValueAtKnownPos(all, curSelection, knownPos)
      mutableList += ((curSelection, valueAtCurPos))
      all = all filter { el ⇒ el(curSelection) == valueAtCurPos }
    }

    { mutableList.toList }.sortBy(x ⇒ x._1) map { x ⇒ x._2 }
  }

  def allNosAtPos(p : Int, all : List[List[Int]], known : Int) : List[(Int, Boolean)] = {
    for (el ← all) yield {
      (el(p), el(known) == 1)
    }
  }

  def createFreqTable(tuples : List[(Int, Boolean)]) : List[(Int, Int, Boolean)] = {
    val table = mutable.Map[Int, (Int, Boolean)]()
    for (x ← tuples) {
      table.get(x._1) match {
        case None ⇒
          table += x._1 → (1, x._2)
        case Some(entry) ⇒
          table += x._1 → (1 + entry._1, x._2 | entry._2)
      }
    }

    table.toList map { z ⇒ (z._1, z._2._1, z._2._2) } sortBy (x ⇒ -x._2)
  }

  def findHighestVaritionWithAKnownValueAtKnownPos(all : List[List[Int]], p : Int, known : Int) : Int = {

    val allAtPos : List[(Int, Boolean)] = allNosAtPos(p, all, known)
    val freqTables : List[(Int, Int, Boolean)] = createFreqTable(allAtPos)
    val candidate = { freqTables filter (z ⇒ z._3 == true) }.head
    candidate._1
  }

}

class ProblemCTest extends FunSuite {
  test ("1") {
    for (i ← Range(6, 7)) {
      val res = ProblemD(i).getValidPerms
      res foreach (println(_))
      println(res.size)
    }
  }

  test("2") {
    val res1 = ProblemD(6).solve(List(1, 2, 3, 4, 5, 6))
    println(res1)
    val res2 = ProblemD(6).solve(List(1, 2, 3, 4, 6, 5))
    println(res2)
    val res3 = ProblemD(6).solve(List(1, 2, 3, 6, 5, 4))
    println(res3)
    val res4 = ProblemD(6).solve(List(1, 2, 6, 3, 5, 4))
    println(res4)
    val res5 = ProblemD(6).solve(List(6, 2, 1, 3, 5, 4))
    println(res5)
    val res6 = ProblemD(6).solve(List(6, 3, 2, 1, 5, 4))
    println(res6)

  }
}
