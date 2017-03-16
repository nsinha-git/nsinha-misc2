package com.nsinha.problems.GoogleJam.ZeroEight.WorldFinal

import com.nsinha.common.{Block, Coordinate, Grid}
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/15/17.
  */
object Forest extends GridValue
object Island extends GridValue
object Water extends GridValue
case class Weight(g : GridValue, w : Int)

case class ProblemD(rows : Int, cols : Int, input : String) {

  val grid = new Grid(rows, cols)
  val gridValues = mutable.Map[Block, Weight]()
  val visited = mutable.Set[Block]()

  processInput

  private def processInput = {
    var curRow = 0
    val inps = input.split("\n").toList
    inps map {
      str ⇒
        Range(0, cols) map {
          curCol ⇒
            val gridValue = getGridValue(str(curCol))
            val curBlock = grid.blocks(Coordinate(curRow, curCol))
            gridValues += (curBlock → gridValue)
        }
        curRow = curRow + 1
    }
  }

  private def getGridValue(c : Char) : Weight = {
    c match {
      case 'T' ⇒ Weight(Forest, Integer.MAX_VALUE)
      case '.' ⇒ Weight(Water, Integer.MAX_VALUE)
      case '#' ⇒ Weight(Island, Integer.MAX_VALUE)
    }
  }

  def findCost : Int = {
    doBfs
    doBfs
    aggregate
  }

  private def aggregate = {
    gridValues.foldLeft(0){ (Z, el) ⇒
      if (el._2.g != Water) el._2.w + Z else Z
    }
  }

  private def doBfs = {
    val que = mutable.Queue[Block]()
    visited.clear()
    que += (grid.blocks(Coordinate(0, 0)))
    gridValues(grid.blocks(Coordinate(0, 0))) = Weight(Forest, 0)

    doBfsInt(que)
  }

  private def doBfsInt(que : mutable.Queue[Block]) : Unit = {
    if (que.isEmpty) return

    val curBlock = que.dequeue()

    visited += curBlock

    val allEdges = grid.getAllSurroundingEdgesForBlock(curBlock)
    var nBlks = mutable.Set[Block]()
    allEdges foreach {
      edge ⇒
        grid.getAllSurroundingBlocksForEdge(edge) foreach (nBlks += _)
    }
    nBlks = nBlks.diff(Set(curBlock))
    val nBlksNoWater = nBlks filter (x ⇒ gridValues(x).g != Water)
    var curWeight = if (gridValues(curBlock).g == Forest) 0 else gridValues(curBlock).w
    val childWt = curWeight + 1
    nBlksNoWater foreach { nBlk ⇒
      if (gridValues(nBlk).w > childWt) {
        gridValues(nBlk) = Weight(gridValues(nBlk).g, childWt)
      }
      if (!visited.contains(nBlk)) que += nBlk
    }
    doBfsInt(que)
  }
}

class TestProblemD extends FunSuite {

  test("a") {
    val p = ProblemD(2, 2,
      """T.
        |T#""".stripMargin)

    println(p.findCost)
  }
  test("b") {
    val p = ProblemD(4, 4,
      """T##.
        |##.#
        |.#T#
        |####""".stripMargin)

    println(p.findCost)
  }

}
