package com.nsinha.problems.GoogleJam.Eleven.WorldFinal.ProblemB

import com.nsinha.common.Coordinate
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 6/8/17.
  */

case object Common {
  type Height = Int
  type Time = Int

}
import Common._

case class Edge(delayToSea : Time, initHeight : Height, prelude : List[Height], constantDec : Height, postLude : Height = 0) {

  val heightAfterPrelude = initHeight - prelude.foldLeft(0) { (Z, el) ⇒ Z + el }
  val stepsPrelude = prelude.size
  val constantDecSteps : Int = {
    if (constantDec != 0) heightAfterPrelude / constantDec else 0
  }
  def getTimeToZero : Time = {
    if (postLude == 0) delayToSea + stepsPrelude + constantDecSteps else delayToSea + constantDecSteps + 1
  }
}

case object EdgeOrdering extends Ordering[Edge] {

  override def compare(x : Edge, y : Edge) : Int = {
    val timeToZeroX = x.getTimeToZero
    val timeToZeroY = y.getTimeToZero
    if (timeToZeroX < timeToZeroY) return -1
    if (timeToZeroY < timeToZeroX) return 1
    if (x.postLude < y.postLude) return -1
    if (x.postLude > y.postLude) return 1
    return 0
  }

}
class ProblemBDijstra(rows : Int, cols : Int, M : Int, input : String) {

  val edgeToCellMap = mutable.Map[Edge, Coordinate]()
  val visitedCells = mutable.Set[Coordinate]()
  val (initialMap, maxHeight) = getInitialMap
  val dijstraPriorityQue = createInitialQueue

  def insertIntoQueRowOrCol(axisNo : Int, que : mutable.PriorityQueue[Edge], rowOrCol : Boolean) = {

    val allCells = if (rowOrCol) initialMap filter (entry ⇒ entry._1.x == axisNo) else initialMap filter (entry ⇒ entry._1.y == axisNo)

    allCells foreach {
      case (cell, height) ⇒
        val edge = if (height < M) {
          Edge(0, height, Nil, 0, height)
        }
        else {
          Edge(0, height, Nil, M, height % M)
        }
        edgeToCellMap += edge → cell
        que += edge
    }
  }

  def createInitialQueue : mutable.PriorityQueue[Edge] = {
    //account for first and last row and first and last col.
    implicit val edgeOrdering = EdgeOrdering
    val que = mutable.PriorityQueue[Edge]()
    insertIntoQueRowOrCol(0, que, true)
    insertIntoQueRowOrCol(rows - 1, que, true)
    insertIntoQueRowOrCol(0, que, false)
    insertIntoQueRowOrCol(cols - 1, que, false)
    que
  }

  def getInitialMap : (Map[Coordinate, Height], Height) = {
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
    null
  }

  def solve : Int = {
    // we have a dijkstra pQue
    //we will pick top element in queue and figure out the neighbor.
    //we will update the neighbor vist state when queueing out.

    while (dijstraPriorityQue.nonEmpty) {
      val curEdge = dijstraPriorityQue.dequeue()
      val curCell = edgeToCellMap(curEdge)

      val curCellHt = initialMap(curCell)
      //the edge only starts affecting after the sea connection is established. which is after seadelay of edge.
      //after delay account, one can look for the first time the edge ht falls below the curHt.
      //let that be at t1.
      // if t1 is in prelude: then for every term we should be able to calculate the cooresponding height loss and
      //remaining height of cell. Once into the constSteps let ht of cur cell is h1 and ht of edge src cell is h2 and
      //h2 < h1. In this case the const step is minimum M and we can decrement curCell also by M as long as it is non Zero
      //and greater than M. If it reaches such value before edge cell contant steps then we can ignore edge cell further and
      // calculate conststeps and postlude of curCell without any other external input.
      //In other case we finish upto constSteps and entrer postlude and
      //e,g 1000 and 101 with M=5
      //900 and 1 after 20 conststeps
      //at 21st step 1 will be 0 and 900 will be 895.
      //consider 105 and 101
      //5 and 1 after 20 steps
      //in 21st step: 1 will be 0 and 5 will be 1
      // so only special case could be when the height of edge cell reduces by less than const steps in postlude.
      //105 101 => 101 96 => 96 91 ..=> 6 1 =>1 0 => 0 0

    }

    1

  }

}

class TestingProblemBDijstra extends FunSuite {

  test("a") {
    val prb = ProblemB(3, 6, 3, "3 8 10 11 10 8\n7 5 2 12 8 8\n6 9 11 9 8 4")
    val res = prb.solve
    println(res)

  }

}
