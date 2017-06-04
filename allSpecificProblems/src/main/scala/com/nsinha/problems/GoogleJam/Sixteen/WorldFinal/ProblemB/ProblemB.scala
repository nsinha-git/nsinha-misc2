package com.nsinha.problems.GoogleJam.Sixteen.WorldFinal.ProblemB

import com.nsinha.common.Ratio
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 5/26/17.
  */

case class ProblemB(totalRooms : Int, roomAdd1 : Int) {
  var room = roomAdd1 - 1

  val dpTable = mutable.HashMap[String, Ratio]()

  val startState = {
    val bset = new Array[Boolean](totalRooms)
    for (i ← Range(0, totalRooms))
      bset(i) = false
    bset
  }

  evaluate

  def findAllPairsEmpty(state : Array[Boolean]) : List[(Int, Int)] = {
    var lastEmpty = -1
    val resList = mutable.MutableList[(Int, Int)]()
    for (i ← Range(0, state.size)) {
      val x = state(i)

      if (x == false) {
        if (lastEmpty >= 0 & (lastEmpty + 1) == i) {
          resList += ((lastEmpty, i))
        }
        lastEmpty = i
      }
    }

    resList toList
  }

  def evaluate : Ratio = {
    val res = evaluate(startState)
    println(turnIntoModRemainder(res))
    res
  }

  def evaluate(state : Array[Boolean]) : Ratio = {

    if (dpTable.contains(getString(state))) return dpTable(getString(state))
    if (state(room)) {
      dpTable(getString(state)) = new Ratio(1, 1)
      return dpTable(getString(state))
    }

    val allPairs = findAllPairsEmpty(state)

    var sumRatio = new Ratio(0, 1)

    if (allPairs.nonEmpty) {
      val ratioOfProbForEachPair = new Ratio(1, allPairs.size)

      allPairs foreach {
        case (k, l) ⇒

          val newBitSet = {
            val bSet = state.clone()
            bSet(k) = true
            bSet(l) = true
            bSet
          }

          val ratio = evaluate(newBitSet)

          sumRatio = sumRatio.+ (ratio.*(ratioOfProbForEachPair))
      }

      dpTable += getString(state) → sumRatio
      sumRatio
    }
    else {
      val ratio = if (state(room)) new Ratio(1, 1) else new Ratio(0, 1)
      dpTable(getString(state)) = ratio
      ratio
    }
  }

  def getString(s : Array[Boolean]) : String = {
    var acc = ""
    for (i ← Range(0, s.size)) {
      acc = acc + {
        if (s(i)) "T" else "F"
      }
    }
    acc
  }

  def turnIntoModRemainder(r : Ratio) : Long = {
    val num = r.num
    val denum = r.denum
    val mod : Int = 1000000000 + 7
    if (num == denum) return 1

    for (m ← Range(1, mod)) {
      if ((m * mod + num) % denum == 0) {
        return (m * mod + num) / denum
      }
    }
    mod
  }

}

class ProblemBTetsting extends FunSuite {

  test ("a") {
    ProblemB(4, 1)
    ProblemB(4, 2)
    ProblemB(3, 1)
    ProblemB(3, 2)

  }

}
