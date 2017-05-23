package com.nsinha.problems.GoogleJam.Fifteen.WorldFinal.ProblemA

import scala.collection.mutable

/** Created by nsinha on 5/17/17.
  */
object Common {
  type LevelCost = Int
  type CostInsideLevel = Int
  type Cost = Int
  type Pos = Int

}
import Common._
class ProblemA(costArr : Array[LevelCost]) {

  val passThroughData = new Array[LevelCost](costGroups.size)

  case class DPEntry(mylevel : LevelCost, costInsideLevel : CostInsideLevel)

  val costGroups = groupPosAsPerCost //the size of cost group is max 9 keys.
  val dpTable = new Array[DPEntry](costArr.length)
  createInitialDPtable

  def groupPosAsPerCost : mutable.Map[LevelCost, Array[Pos]] = {
    val mp = mutable.TreeMap[LevelCost, mutable.MutableList[Pos]]()

    costArr zip Range(0, costArr.length) foreach {
      case (levelCost, pos) ⇒
        if (!mp.contains(levelCost)) { mp += levelCost → mutable.MutableList[Pos]() }
        mp(levelCost) += pos
    }

    mp map { x ⇒ x._1 → x._2.toArray }
  }

  def compare(x : Pos, y : Pos) : Int = {
    if (x == y) return 0
    if (x < y) return -1
    return 1
  }

  def findTheCostOfSearchInsideThisLevelNormalized(pos : Pos, array : Array[Pos]) : CostInsideLevel = {
    var steps : CostInsideLevel = 0
    var left = 0
    var right = array.length
    var cond = true
    steps = steps + 1
    var leftCompare = compare(array(left), pos)
    if (leftCompare == 0) return steps
    steps = steps + 1
    var rightCompare = compare(array(right), pos)
    if (rightCompare == 0) return steps

    while (cond) { // invariant is number is always greater than left and smaller than right
      val mid = (left + right) / 2
      steps = steps + 1
      var midCompare = compare(array(mid), pos)
      if (midCompare == 0) return steps
      if (midCompare == -1)
        left = mid
      else
        right = mid
    }

    steps
  }

  def createInitialDPtable = {
    costGroups foreach { level ⇒
      val levelCost = level._1
      val levelPos = level._2
      levelPos foreach { pos ⇒
        val localCost : CostInsideLevel = findTheCostOfSearchInsideThisLevelNormalized(pos, levelPos)
        dpTable.update(pos, DPEntry(levelCost, localCost * levelCost))
      }
    }
  }

  def sumTheLevelsTill(array : Array[LevelCost], lastLevel : LevelCost) : Cost = {
    var sum : Cost = 0
    for (i ← Range(0, lastLevel)) {
      sum = sum + array(i)
    }
    sum
  }

  def passThroughDpTable : Cost = {
    for (i ← Range(0, passThroughData.length)) { //note this works only because a passThroughData can t have more than 9 levels as every level is a single digit.
      passThroughData.update(i, 0)
    }

    var totalCostWorst : Cost = 0

    for (i ← Range(0, dpTable.length)) {
      val curEntry = dpTable(i)
      val curLevel = curEntry.mylevel
      val curLocalCost = curEntry.costInsideLevel
      passThroughData.update(curLevel, curLocalCost)
      val cost = sumTheLevelsTill(passThroughData, curLevel)
      if (cost > totalCostWorst) totalCostWorst = cost
    }

    totalCostWorst

  }

}

