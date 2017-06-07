package com.nsinha.problems.GoogleJam.Twelve.WorldFinal.ProblemB

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 6/4/17.
  */
case class ProblemB(probsAndCnt : Map[Double, Int], k : Int, curProbStoA : Double = 0, probA : Double = 1, path : List[Double] = Nil) {
  val probB = 1 - probA
  val listOfProbs = {
    val res = probsAndCnt.foldLeft(mutable.MutableList[Double]()) {
      (Z, el) ⇒
        for (i ← Range(0, el._2)) {
          Z += el._1
        }
        Z
    }
    res.toList
  }

  val listOfProbsAsc = listOfProbs.sorted
  val listOfProbsDsc = listOfProbs.sortBy(1 / _)

  def solve : (Double, List[Double]) = {
    //we can assume k is lesser than total probablities.

    val initialCandidates = listOfProbsDsc.take(k - 1) ++ listOfProbsAsc.take(1)
    var curlistOfProbAsc = listOfProbsAsc.drop(1)
    val mutableListOfCandidates = mutable.MutableList[Double]()
    mutableListOfCandidates += initialCandidates(initialCandidates.size - 1)

    //we now need to run a back run on lower Probs and break at some point or before we reach the top
    var cond = true
    for (cnt ← Range(1, initialCandidates.size - 1)) {
      if (cond) {
        val candidate = curlistOfProbAsc.head
        val target = initialCandidates(initialCandidates.size - cnt - 1)
        val beforeTarget = initialCandidates(initialCandidates.size - 2 - cnt)
        val afterTarget = initialCandidates(initialCandidates.size - cnt)

        val targetCurSToA = (1 - beforeTarget) * target + (1 - target) * afterTarget
        val candStoA = (1 - beforeTarget) * candidate + (1 - candidate) * afterTarget

        if (candStoA < targetCurSToA) {
          mutableListOfCandidates += candidate
          curlistOfProbAsc = curlistOfProbAsc.drop(1)
        }
        else {
          cond = false
          //we should copy all the remaining of initaialCandidates starting from cur target to one before last.
          for (cnt2 ← Range(cnt, initialCandidates.size - 1)) {
            mutableListOfCandidates += initialCandidates(initialCandidates.size - cnt2 - 1)
          }

        }
      }
      else {

      }
    }

    mutableListOfCandidates += initialCandidates(0)

    val bestProbList = mutableListOfCandidates.reverse.toList
    var totalProb = 0d

    for (cnt ← Range(1, bestProbList.size)) {
      val prev = bestProbList(cnt - 1)
      val cur = bestProbList(cnt)
      totalProb = totalProb + (1 - prev) * cur
    }

    (totalProb, bestProbList)
  }

  def solveBrute : (Double, List[Double]) = {
    if (k <= 0) return (curProbStoA, path)
    val allprobStoA = {
      for (el ← probsAndCnt) yield {
        val newFreq = el._2 - 1
        val newProbsAndCnt = if (newFreq > 0) probsAndCnt.updated(el._1, newFreq) else (probsAndCnt.-(el._1))
        val toWake = el._1
        val toSleep = 1 - el._1
        val newProbA = toWake
        val newProbB = toSleep
        val newProbStoA = probB * toWake + curProbStoA
        ProblemB(newProbsAndCnt, k - 1, newProbStoA, newProbA, path :+ el._1) solveBrute
      }
    }.toList

    allprobStoA.minBy(_._1)

  }

}

class ProblemBTesting extends FunSuite {

  def printCombinationsProduct(mp : Map[Double, Int]) {

    val a_s = mp.keys.map { x ⇒ (x, 1 - x) } toList

  }

  test("a") {
    val mp = mutable.Map[Double, Int]()
    mp += .5 → 2
    mp += 0.33333 → 2
    mp += 0.26 → 2
    mp += 0.75 → 2

    val res = ProblemB(mp.toMap, 3) solveBrute

    println(res)
  }

  test("b") {
    val mp = mutable.Map[Double, Int]()
    mp += .5 → 2
    mp += 0.33333 → 2
    mp += 0.26 → 2
    mp += 0.75 → 2

    val res = ProblemB(mp.toMap, 3) solve

    println(res)
  }

}
