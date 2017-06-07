package com.nsinha.problems.GoogleJam.Eleven.WorldFinal.ProblemB

import org.scalatest.FunSuite

import scala.collection.immutable
import scala.collection.mutable

/** Created by nsinha on 6/5/17.
  */

trait DecisionState
case object FullyDecided extends DecisionState
case object PartiallyDecided extends DecisionState
case class ProblemB(rows : Int, cols : Int, M : Int, input : String) {

  val (initialMap, maxHeight) = getInitialMap

  def solve : Int = {
    iterateDaysTillAllZeros
  }

  def getInitialMap : (Map[(Int, Int), Int], Int) = {
    val mp = mutable.HashMap[(Int, Int), Int]()
    var maxHeight = 0

    input.split("\n") zip Range(0, rows) map {
      case (str, row) ⇒
        str.split(" ") zip Range(0, cols) map {
          case (s, col) ⇒
            val ht = Integer.parseInt(s)
            mp += (row, col) → ht
            if (ht > maxHeight) maxHeight = ht
        }
    }
    mp.toMap → maxHeight
  }
  def initialMapToWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)] = {
    val mp = mutable.TreeMap[(Int, Int), (Int, Int, DecisionState)]()

    for (el ← initialMap) {
      mp += el._1 → (maxHeight, el._2, PartiallyDecided)
    }
    mp
  }

  def iterateDaysTillAllZeros : Int = {
    var mp = initialMapToWaterLevelMap
    var cond = true
    var days = 0
    while (cond) {
      mp = processWaterLevelCurDay(mp)
      if (checkAllZeroLandHeight(mp)) cond = false else days = days + 1
    }

    days
  }

  def checkAllZeroLandHeight(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : Boolean = {
    curWaterLevelMap forall { el ⇒ el._2._2 == 0 }
  }

  def processWaterLevelCurDay(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) = {

    getWaterLevelMap(curWaterLevelMap)

    getAfterErosionHeightMap(curWaterLevelMap)
  }

  def getMinWaterLevel(nbrs : List[(Int, Int)], curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : Int = {
    var minHeight = maxHeight
    for (nbr ← nbrs) {
      if (nbr._1 < 0 || nbr._1 >= rows) { minHeight = 0 } else if (nbr._2 < 0 || nbr._2 >= cols) { minHeight = 0 } else if (curWaterLevelMap(nbr)._1 < minHeight) minHeight = curWaterLevelMap(nbr)._1
    }
    minHeight
  }

  def getAfterErosionHeightMap(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : mutable.Map[(Int, Int), (Int, Int, DecisionState)] = {
    val mp = mutable.TreeMap[(Int, Int), (Int, Int, DecisionState)]()
    for (cell ← curWaterLevelMap.keys) {
      val levels = curWaterLevelMap(cell)
      var waterLevel = levels._1
      var landLevel = levels._2
      var nbrs = getNbrCoords(cell)
      val minNbrWaterLevel = getMinWaterLevel(nbrs, curWaterLevelMap)
      var erosion = math.min(waterLevel - minNbrWaterLevel, M)
      mp += cell → (maxHeight, landLevel - erosion, PartiallyDecided)
    }

    mp

  }

  def getUndecidedLevelSets(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : mutable.Map[Int, mutable.Set[(Int, Int)]] = {
    val undecidedOnly = curWaterLevelMap filter { case (k, v) ⇒ v._3 == PartiallyDecided }
    val levelWiseSetOfCells = groupIntoLevels(undecidedOnly)
    levelWiseSetOfCells
  }

  def groupIntoLevels(curWaterLevelMapUndecided : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : mutable.Map[Int, mutable.Set[(Int, Int)]] = {

    val aggregateSet = mutable.TreeMap[Int, mutable.Set[(Int, Int)]]()

    for (el ← curWaterLevelMapUndecided) {
      val level = el._2._1

      if (!aggregateSet.contains(level)) {
        aggregateSet += level → mutable.Set[(Int, Int)] ()
      }
      aggregateSet(level) += el._1
    }
    aggregateSet
  }

  def getWaterLevelMap(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) = {
    getWaterLevelMapStep1(curWaterLevelMap)
    var cond = true
    while (cond) {
      if (noUndecidedExists(curWaterLevelMap)) {
        cond = false
      }
      else {
        val undecidedCellLevels = getUndecidedLevelSets(curWaterLevelMap)

        for (level ← undecidedCellLevels.keys.take(1)) {
          val curLevelSet = undecidedCellLevels(level)
          for (cell ← curLevelSet) {
            val cellState = curWaterLevelMap(cell)
            if (cellState._1 <= cellState._2) {
              curWaterLevelMap += cell → (cellState._2, cellState._2, FullyDecided)
            }
            else {
              curWaterLevelMap += cell → (cellState._1, cellState._2, FullyDecided)
            }
          }
          for (cell ← curLevelSet) {
            doDfsFix(cell, curWaterLevelMap)
          }
        }
      }
    }
  }

  def doDfsFix(cell : (Int, Int), curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : Unit = {

    val nbrs = getNbrCoords(cell)

    for (nbr ← nbrs) {
      if (!isFullyDecidedNbr(nbr, curWaterLevelMap)) {
        val decisionState = processACell(nbr, curWaterLevelMap)
        if (decisionState == FullyDecided) {
          val nbrsOfnbr = getNbrCoords(nbr)
          for (nbr2 ← nbrsOfnbr)
            if (!isFullyDecidedNbr(nbr, curWaterLevelMap)) {
              doDfsFix(nbr2, curWaterLevelMap)
            }
        }

      }
    }
  }

  def noUndecidedExists(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : Boolean = {
    curWaterLevelMap.forall{ case (k, v) ⇒ v._3 == FullyDecided }
  }

  def getWaterLevelMapStep1(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) = {
    for (cnt ← Range(0, rows / 2 + 1)) {
      if (cnt < (rows - cnt - 1)) {
        processRowWaterLevelIteration(curWaterLevelMap, cnt)
        processRowWaterLevelIteration(curWaterLevelMap, rows - 1 - cnt)
      }
      else if (cnt == (rows - cnt - 1)) {
        processRowWaterLevelIteration(curWaterLevelMap, cnt)
      }
    }
  }

  def processACell(cell : (Int, Int), curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : DecisionState = {
    val (minHeight, undecidedExists) = getMinimumOfFullyDecidedNeighborsAndExistenceOfUndecidedNbr(cell, curWaterLevelMap)
    val prevHeight = curWaterLevelMap(cell)._1
    val landHeight = curWaterLevelMap(cell)._2
    var decided : DecisionState = FullyDecided
    if (undecidedExists) {
      if (minHeight <= prevHeight) {
        if (minHeight < landHeight) {
          curWaterLevelMap(cell) = (landHeight, landHeight, FullyDecided)
        }
        else {
          decided = PartiallyDecided
          curWaterLevelMap(cell) = (minHeight, landHeight, PartiallyDecided)
        }
      }
    }
    else {
      if (minHeight <= prevHeight) {
        if (minHeight < landHeight) {
          curWaterLevelMap(cell) = (landHeight, landHeight, FullyDecided)
        }
        else {
          curWaterLevelMap(cell) = (minHeight, landHeight, FullyDecided)
        }
      }
    }

    decided

  }

  def processRowWaterLevelIteration(curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)], row : Int) : Unit = {
    val allrowCells = curWaterLevelMap filter { case (r, c) ⇒ r._1 == row }

    for (cell ← allrowCells) {
      processACell(cell._1, curWaterLevelMap)
    }
  }

  def getNbrCoords(coord : (Int, Int)) : List[(Int, Int)] = {
    List ((coord._1 - 1, coord._2), (coord._1 + 1, coord._2), (coord._1, coord._2 - 1), (coord._1, coord._2 + 1))
  }

  def isFullyDecidedNbr(nbr : (Int, Int), curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : Boolean = {
    if (nbr._1 < 0 || nbr._1 >= rows) return true
    if (nbr._2 < 0 || nbr._2 >= cols) return true
    if (curWaterLevelMap(nbr)._3 == FullyDecided) return true
    false
  }

  def checkFullyDecidedNbrs(nbrs : List[(Int, Int)], curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : (List[(Int, Int)], List[(Int, Int)]) = {
    val decided = mutable.MutableList[(Int, Int)]()
    val notdecided = mutable.MutableList[(Int, Int)]()
    for (nbr ← nbrs) {

      if (isFullyDecidedNbr(nbr, curWaterLevelMap)) decided += nbr else notdecided += nbr

    }
    (decided.toList, notdecided.toList)
  }

  def getMinimumOfFullyDecidedNeighborsAndExistenceOfUndecidedNbr(cell : (Int, Int), curWaterLevelMap : mutable.Map[(Int, Int), (Int, Int, DecisionState)]) : (Int, Boolean) = {

    val nbrs = getNbrCoords(cell)

    val (fullyDecidedNbrs, undecidedNbrs) = checkFullyDecidedNbrs(nbrs, curWaterLevelMap)
    val minHeightFullyDecided = fullyDecidedNbrs.foldLeft(maxHeight) {
      (Z, el) ⇒
        if (curWaterLevelMap.contains(el)) {
          if (Z > curWaterLevelMap(el)._1) {
            curWaterLevelMap(el)._1
          }
          else {
            Z
          }
        }
        else {
          0
        }
    }
    val doesAUndecidedNbrExist = undecidedNbrs.nonEmpty

    (minHeightFullyDecided, doesAUndecidedNbrExist)
  }

}

class TestingProblemB extends FunSuite {

  test("a") {
    val prb = ProblemB(3, 6, 3, "3 8 10 11 10 8\n7 5 2 12 8 8\n6 9 11 9 8 4")
    val res = prb.solve
    println(res)

  }

}
