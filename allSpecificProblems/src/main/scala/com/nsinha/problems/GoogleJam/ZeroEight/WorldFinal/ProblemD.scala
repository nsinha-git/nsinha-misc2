package com.nsinha.problems.GoogleJam.ZeroEight.WorldFinal

import com.nsinha.common.{Block, Coordinate, Grid}
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/15/17.
  */
object Forest extends GridValue
object Island extends GridValue
object Water extends GridValue
case class Weight(g : GridValue, w : Int) extends Ordered[Weight] {
  override def compare(y : Weight) : Int = {
    val x = this
    if (x.g == Forest && y.g != Forest) return 1
    if (x.g != Forest && y.g == Forest) return -1
    if (x.w < y.w) return 1
    if (x.w == y.w) return 0
    -1
  }
}
class Time(t : Int)
case class EntryTime(t : Int) extends Time(t)
case class VisitTime(t : Int) extends Time(t)

case class ProblemD(rows : Int, cols : Int, input : String) {
  implicit object QueueOrdering extends Ordering[(Block, Weight, EntryTime)] {
    override def compare(x : (Block, Weight, EntryTime), y : (Block, Weight, EntryTime)) : Int = {
      val t1 = x._2.compare(y._2)
      if (t1 != 0) return t1
      -implicitly[Ordering[Int]].compare(x._3.t, y._3.t)
    }
  }

  val grid = new Grid(rows, cols)
  //gridvalues carry the EntryTime which depicts the eariest time the node got visited by a normal node or latest time from a Forest
  // VisitTime when the node egresses the que
  //Should forest change value for nodes that have egressed the que?
  //Que that contains entered but unvisisted nodes should be arranged order of forest and then their initiation time.
  // Note the difference when in bfs ques are only arranged as per initiation times.
  val gridValues = mutable.Map[Block, Weight]()
  val visited = mutable.Set[Block]()
  val entered = mutable.Set[Block]()
  var que = mutable.PriorityQueue[(Block, Weight, EntryTime)]()
  var curTime = 0

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
            gridValues += (curBlock → (gridValue))
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
    aggregate
  }

  private def aggregate = {
    gridValues.foldLeft(0){ (Z, el) ⇒
      val ell = el._2
      if (ell.g != Water) ell.w + Z else Z
    }
  }

  private def doBfs = {
    visited.clear()
    entered.clear()
    que.clear()
    val blk = grid.blocks(Coordinate(0, 0))
    val newWeight = Weight(Forest, 0)
    que += ((blk, newWeight, EntryTime(0)))
    entered += blk
    gridValues(grid.blocks(Coordinate(0, 0))) = newWeight

    doBfsInt
  }

  private def doBfsInt : Unit = {
    curTime = curTime + 1
    if (que.isEmpty) return

    val curQueElem = que.dequeue()

    val curBlock = curQueElem._1

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
      if (!visited.contains(nBlk)) {
        val prevWeight = gridValues(nBlk).w
        if (prevWeight > childWt) {
          gridValues(nBlk) = Weight(gridValues(nBlk).g, childWt)
          //is bBlk guranteed to be in que. no if it's seen for first time
          if (!entered.contains(nBlk)) {
            que += ((nBlk, gridValues(nBlk), EntryTime(curTime)))
            entered += nBlk
          }
          else { //nBlk is already in Que but now it's weight changed. We need to remove the element.Priority Que seems unsuitable for
            //this op. A direct Heap or fibonnacci heap may be better. But let's note this down and move.
            que = que filter (x ⇒ x._1 != nBlk)
            que += ((nBlk, gridValues(nBlk), EntryTime(curTime)))
          }
        }
      }
    }
    doBfsInt
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
