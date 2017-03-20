package com.nsinha.problems.GoogleJam.ZeroEight.WorldFinal

import com.nsinha.common.{Block, Grid}
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/17/17.
  */
object ProblemC {

  def getTheSurroundingBlocksIncludeingSWNWNESE(g : Grid, b : Block) : Set[Block] = {
    val allGps = g.getAllSurroundingGridPointsForBlock(b)
    val surroundinBlocks = allGps.foldLeft(Set[Block]()) { (Z, el) ⇒
      val x = g.getAllSurroundingBlocksForGridPoint(el)
      Z ++ x
    }
    surroundinBlocks
  }

  def getTheGridWithNumOfNeighbors(g : Grid) : Map[Block, Int] = {
    g.blocks map {
      x ⇒
        val surroundinBlocks = getTheSurroundingBlocksIncludeingSWNWNESE(g, x._2)
        (x._2, surroundinBlocks.size)
    }
  }

  def getSetOfFreeBlocks(s : String) : Set[Int] = {
    val allRows = s.split("\n")
    allRows.foldLeft(0, Set[Int]()) {
      (Z, el) ⇒
        val curSet = el.foldLeft(0, Set[Int]()) { (ZZ, ell) ⇒
          if (ell == '0') {
            (ZZ._1 + 1, ZZ._2 + ZZ._1)
          }
          else {
            (ZZ._1 + 1, ZZ._2)
          }
        }._2 map { x ⇒ x + Z._1 }
        (Z._1 + el.size, Z._2 ++ curSet)
    }._2
  }

}

case class ProblemCBruteForce(rows : Int, cols : Int, inputs : String) {

  val grid = new Grid(rows, cols)

  val initialgridValues = {
    val res = mutable.Map[Block, Int]()
    var blkCnt = 0
    inputs.split("\n") foreach {
      x ⇒
        x foreach { char ⇒
          if (char != ' ') {
            res += grid.getTheBlockNumbered(blkCnt) → (char.toInt - 48)
            blkCnt = blkCnt + 1
          }
        }
    }
    res.toMap
  }

  val prob = ProblemCBruteForceInt(grid, initialgridValues)

  prob.zeroMaps
}

case class ProblemCBruteForceInt(grid : Grid, gridValues : Map[Block, Int]) {

  val dpTable : mutable.HashMap[Int, mutable.HashMap[String, Map[Block, Int]]] = mutable.HashMap[Int, mutable.HashMap[String, Map[Block, Int]]]()
  val zeroMaps : mutable.Set[String] = mutable.Set[String]()
  val canTry : mutable.Set[Block] = mutable.Set[Block]()

  val allZeroStringForMines : String = {
    val aLine : String = { Range(0, grid.cols) map { x ⇒ '0' } }.mkString("")
    val res = Range(0, grid.rows) map { x ⇒ aLine }
    res.mkString("\n")
  }

  val sumOfAllGrids = gridValues.foldLeft(0) { (Z, el) ⇒ Z + el._2 }

  //we are going to take mines from aboutRange before it finishes. If no success till then we are finished with failure.
  //else if we get success may be with few balls remaining we are still good.
  val aboutRange : Int = ((sumOfAllGrids + grid.rows * 2 + grid.cols * 2 + 4) * (1.2 / 9)).toInt;

  val blockToNumOfNeighborsMap : Map[Block, Int] = ProblemC.getTheGridWithNumOfNeighbors(grid)

  tryFullSearch

  def tryFullSearch = {
    initDpTable()
    var stage = 2
    var cond = true
    while (cond == true) {
      cond = fillDpTable(stage)
      stage = stage + 1
    }
    if (zeroMaps.nonEmpty) {
      println(s"${zeroMaps.size} sols found.")
      zeroMaps foreach (x ⇒ println(x+"\n"))

    }
    else {
      println("No sol found")
    }
  }

  def initDpTable() = {
    dpTable += 1 → mutable.HashMap[String, Map[Block, Int]]()
    for (blk ← grid.blocks) {
      val res = evaluate(allZeroStringForMines, gridValues, blk._2)
      if (!res._3) {
        dpTable(1) += res._1 → res._2
        canTry += blk._2
      }
    }
  }

  def fillDpTable(stage : Int) : Boolean = {
    //stage n+1 depends on stage n
    dpTable += stage → mutable.HashMap[String, Map[Block, Int]]()
    dpTable filter (x ⇒ x._1 == (stage - 1)) map {
      configTable ⇒
        val innerTable = configTable._2
        innerTable filter { x ⇒ x._2.nonEmpty } map { x ⇒
          val setOfFreeBlocks : Set[Int] = ProblemC.getSetOfFreeBlocks(x._1)

          setOfFreeBlocks foreach { freeBlock ⇒
            if (canTry.contains(grid.getTheBlockNumbered(freeBlock))) {
              val res = evaluate(x._1, x._2, freeBlock)
              if (!res._3) dpTable(stage) += (res._1 → res._2)
            }
          }
        }
    }
    println(s"stage = $stage sizeOfDp = ${dpTable(stage).size}")
    dpTable(stage).nonEmpty
  }

  def evaluate(s : String, mp : Map[Block, Int], n : Int) : (String, Map[Block, Int], Boolean) = {
    val blk = grid.getTheBlockNumbered(n)
    evaluate(s, mp, blk)
  }

  def evaluate(s : String, mp : Map[Block, Int], blk : Block) : (String, Map[Block, Int], Boolean) = {
    assert(blk != null)
    val newS = changeBlockStringAtRowCol(blk.x, blk.y, s)
    val allBlks = ProblemC.getTheSurroundingBlocksIncludeingSWNWNESE(grid, blk)
    var fail = false
    //alter the map of all these
    val resMap = mp map { x ⇒
      if (allBlks.contains(x._1)) {
        if (x._2 - 1 < 0) fail = true
        x._1 → (x._2 - 1)
      }
      else
        x
    }
    var mpSum = 0
    resMap foreach (x ⇒ mpSum = x._2 + mpSum)
    if (mpSum == 0 && !fail) {
      zeroMaps += newS
      //println(s)
      //println(mp)
      //println(s"blk :${blk.x} ${blk.y}")
      //println("New Orientation")
      //println(newS)
      //println()
      //println(resMap)
      //println()

    }

    (newS, resMap, fail)
  }

  def changeBlockStringAtRowCol(row : Int, col : Int, s : String) : String = {
    val sArr = s.split("\n")
    val toChange = sArr(row)
    val changed = toChange.foldLeft(List[Char](), 0) { (Z, el) ⇒
      val char = if (Z._2 == col) {
        'x'
      }
      else {
        el
      }
      (Z._1 :+ char, Z._2 + 1)
    }._1.mkString("")
    sArr(row) = changed
    sArr.mkString("\n")
  }
}

class TestingC extends FunSuite {

  test("a") {
    ProblemCBruteForce(5, 4,
      """4 6 5 3
        |6 9 8 5
        |6 8 8 5
        |4 6 7 5
        |2 3 4 3""".stripMargin)

  }

  test("b") {
    val str =
      """0xx0
        |xxxx
        |xxxx
        |0x0x
        |x0xx""".stripMargin

    val set = ProblemC.getSetOfFreeBlocks(str)
    print(set)

  }

}

