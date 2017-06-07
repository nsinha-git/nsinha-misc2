package com.nsinha.problems.GoogleJam.Thirteen.WorldFinal.ProblemB

import org.scalatest.FunSuite

/** Created by nsinha on 6/4/17.
  */
case class ProblemB(l : List[Double], offSetMaxRange : Double = -1) {

  def findMaxDiffAndIndex(ll : List[Double]) : (Double, Int, Int) = {
    val max = ll map { math.abs(_) } max
    val (index, sign) = { ll zip Range(0, ll.size) }.foldLeft(0, 1) { (Z, el) ⇒
      if (max == math.abs(el._1)) {
        (math.abs(el._2), if (el._1 != math.abs(el._1)) -1 else 1)
      }
      else Z

    }
    (max, index, sign)
  }

  def getT0 : Double = {
    if (l.nonEmpty) l.head else 0
  }

  def solveTop : Double = {
    val t0 = getT0
    var cond = true
    var ltemp = l map { _ - t0 }
    val maxErrForZeroOffset = ProblemB(ltemp) solve
    var curOffSet = maxErrForZeroOffset
    var lastErr = maxErrForZeroOffset
    ltemp = ltemp.slice(0, ltemp.size)
    var cnt = 0
    while (cond) {
      var minForThisOffsetPos = ProblemB(ltemp.updated(0, ltemp(0) + curOffSet), curOffSet) solve
      var minForThisOffsetNeg = ProblemB(ltemp.updated(0, ltemp(0) - curOffSet), curOffSet) solve

      //convex conditions. Only one should be true
      if (minForThisOffsetPos < lastErr) {
        lastErr = minForThisOffsetPos
        ltemp = ltemp.updated(0, ltemp(0) + curOffSet)
        curOffSet = curOffSet / 2
        cnt = 0
      }
      else if (minForThisOffsetNeg < lastErr) {
        lastErr = minForThisOffsetNeg
        ltemp = ltemp.updated(0, ltemp(0) - curOffSet)
        cnt = 0
      }
      else {
        //no improvement so we should just reduce the currentOffset
        curOffSet = curOffSet / 2
      }
      cnt = cnt + 1
      if (cnt > 20) cond = false
    }
    //ltemp has the max error
    lastErr
  }

  def solve : Double = {
    val (maxD, indexMax, sign) = findMaxDiffAndIndex(l)
    var gTry = (sign * maxD * 1.0f - l(0)) / indexMax
    var cond = true
    var cnt = 0

    while (cond) {
      if (goodChoice(gTry, maxD)) cond = false
      if (cnt > 20) cond = false
      cnt = cnt + 1
      if (cond) gTry = gTry / 2
    }

    if (cnt > 20) {
      maxD
    }
    else {
      println(gTry)
      ProblemB (l zip Range(0, l.size) map { el ⇒ el._1 - l(0) - el._2 * gTry }) solve
    }

  }

  def goodChoice(gTry : Double, max : Double) : Boolean = {
    val ll = l zip Range(0, l.size) map (z ⇒ (z._1 - z._2 * gTry - l(0)))
    val (curMax, curIndex, sign) = findMaxDiffAndIndex(ll)

    if (curMax < max) true else false
  }
}

class ProblemBTetsting extends FunSuite {

  test("a"){
    val prb = ProblemB(List(2, 5, 10, 15, 20, 24))
    println(prb.solveTop)
    println(prb)
  }

}
