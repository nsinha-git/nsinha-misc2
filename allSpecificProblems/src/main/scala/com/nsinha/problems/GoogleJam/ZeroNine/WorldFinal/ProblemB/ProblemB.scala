package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemB

import com.nsinha.common.Coordinate

import scala.collection.mutable
import java.lang.Integer.parseInt

/** Created by nsinha on 4/11/17.
  */
class ProblemB(inputs : String) {

  val allPointsSet = mutable.HashSet[Coordinate]()
  processInputs
  val problem = ProblemBSolve(allPointsSet)
  //problem.print

  def processInputs = {
    val lines = inputs.split("\n")
    for (line ← lines) {
      val pt = line.split(" ") map { parseInt(_) }
      val co = Coordinate(pt(0), pt(1))
      allPointsSet += (co)
    }
  }
}

case class ProblemBSolve(points : mutable.HashSet[Coordinate]) {

  val pointsXAxisSorted = points.toArray.sortBy(λ ⇒ λ.x)
  val length = pointsXAxisSorted.length

  var fullyEvalDistanceMatrix = mutable.ResizableArray[Array[Double]]()

  def findSplitXAxis = {
    if (length > 2) pointsXAxisSorted(length / 2).x else pointsXAxisSorted(0).x
  }

  def findSolution : Int = {
    if (length < 30) {
      fullyEval
    }
    else {
      1
    }
  }

  def distance(coordinate1 : Coordinate, coordinate2 : Coordinate) : Double = {
    val xoff = coordinate1.x - coordinate2.x
    val yoff = coordinate1.y - coordinate2.y
    math.sqrt(xoff * xoff + yoff + yoff)
  }

  def fullyEval : Int = {
    //for each point create lines to remaning n points. line just means length.
    //to remain symmetric a point is allowed to have length with itself. we will disallow points with
    // zero sides to take care of this issue.
    /* pointsXAxisSorted zip Range(0, pointsXAxisSorted.length) foreach { case (curRowCoord,i) =>
      fullyEvalDistanceMatrix :+  (pointsXAxisSorted map {secondPt => distance(curRowCoord, secondPt)})
    }*/
    //eval all triangles

    1

  }

}

