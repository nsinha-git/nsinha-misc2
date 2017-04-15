package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemB

import com.nsinha.common.{Coordinate, PermutationCombination}

import scala.collection.mutable
import java.lang.Integer.parseInt

import org.scalatest.FunSuite

import scala.util.Random

/** Created by nsinha on 4/11/17.
  */

case class ProblemB(inputs : Array[Coordinate]) {
  val allPointsSet = mutable.HashSet[Coordinate]()
  processInputs
  val distanceMap = mutable.HashMap[Set[Coordinate], Double]()
  val triangleMap = mutable.HashMap[Set[Coordinate], Double]()
  val problem = ProblemBSolve(allPointsSet, distanceMap, triangleMap)
  println(problem.findSolution)

  def processInputs = {
    for (line ← inputs) {
      allPointsSet += (line)
    }
  }
}

case class ProblemBSolve(points : mutable.HashSet[Coordinate], val distanceMap : mutable.HashMap[Set[Coordinate], Double],
                         val triangleMap : mutable.HashMap[Set[Coordinate], Double]) {

  val pointsXAxisSorted = points.toArray.sortBy(λ ⇒ λ.x)
  lazy val pointsYAxisSorted = points.toArray.sortBy(λ ⇒ λ.y)
  val length = pointsXAxisSorted.length

  private def findSplitXAxis = {
    if (length > 2) pointsXAxisSorted(length / 2).x else pointsXAxisSorted(0).x
  }

  private def findSplitYAxis = {
    if (length > 2) pointsYAxisSorted(length / 2).y else pointsYAxisSorted(0).y
  }

  def findSolution : Double = {
    if (length < 30) {
      fullyEval(pointsXAxisSorted) match {
        case Some(v) ⇒ v
        case None    ⇒ Double.MaxValue
      }
    }
    else {
      val splitX = findSplitXAxis
      val pMinTwoSides = Math.min(ProblemBSolve(points.filter(λ ⇒ λ.x <= splitX), distanceMap, triangleMap) findSolution, ProblemBSolve(points.filter(λ ⇒ λ.x <= splitX), distanceMap, triangleMap) findSolution)
      //we need to merge the points near the splitaxis st all points falling in the near region are taken in this calc.
      val interestingPointsNearAxis = pointsXAxisSorted filter (x ⇒ math.abs(x.x - splitX) <= pMinTwoSides / 2)
      processMergePoints(interestingPointsNearAxis, pMinTwoSides)
    }
  }

  def processMergePoints(pointsInp : Array[Coordinate], twoTimesLimit : Double) : Double = {
    val allMergedSorted = pointsInp.sortBy(p ⇒ p.y)
    val mutableAllMergedSorted = allMergedSorted.foldLeft(mutable.ArrayBuffer[Coordinate]()) { (Z, el) ⇒ Z += el }

    var minimumfound = twoTimesLimit
    allMergedSorted foreach { curP ⇒
      val pointsInCurBlock = gatherPoints(mutableAllMergedSorted, curP.y, curP.y + minimumfound / 2)
      val minValueThisBoxOpt = fullyEval(pointsInCurBlock)
      minValueThisBoxOpt map { minValueThisBox ⇒
        minimumfound = if (minValueThisBox < minimumfound) minValueThisBox else minimumfound
      }
    }
    minimumfound
  }

  def gatherPoints(inPoints : mutable.ArrayBuffer[Coordinate], startY : Int, endY : Double) : Array[Coordinate] = {
    val res = mutable.ArrayBuffer[Coordinate]()
    val removedOnes = mutable.ArrayBuffer[Int]()
    inPoints zip Range(0, inPoints.length) foreach {
      case (pt, index) ⇒
        if (pt.y == startY) removedOnes += index
        if (pt.y <= endY) res += pt
    }
    removedOnes foreach (indx ⇒ inPoints.remove(indx))
    res.toArray
  }

  def distance(coordinate1 : Coordinate, coordinate2 : Coordinate) : Double = {
    if (distanceMap.contains(List(coordinate1, coordinate2).toSet)) {
      return distanceMap(List(coordinate1, coordinate2).toSet)
    }
    val xoff = coordinate1.x - coordinate2.x
    val yoff = coordinate1.y - coordinate2.y
    distanceMap += (List(coordinate1, coordinate2).toSet) → math.sqrt(xoff * xoff + yoff + yoff)
    distanceMap(List(coordinate1, coordinate2).toSet)
  }

  def toSetOfPoints(set : Set[Int], evalPoints : Array[Coordinate]) : Set[Coordinate] = {
    set map { x ⇒ evalPoints(x) }
  }

  def perimeterofTriangle(set : Set[Int], evalPoints : Array[Coordinate]) : Double = {
    val setOfPoints = toSetOfPoints(set, evalPoints)
    val setOfPointsArray = setOfPoints.toArray
    if (triangleMap.contains(setOfPoints)) {
      return triangleMap(setOfPoints)
    }
    triangleMap(setOfPoints) = distance(setOfPointsArray(0), setOfPointsArray(1)) + distance(setOfPointsArray(1), setOfPointsArray(2)) +
      distance(setOfPointsArray(0), setOfPointsArray(2))
    triangleMap(setOfPoints)
  }

  def fullyEval(evalPoints : Array[Coordinate]) : Option[Double] = {
    //for each point create lines to remaning n points. line just means length.
    //to remain symmetric a point is allowed to have length with itself. we will disallow points with
    // zero sides to take care of this issue.
    /* pointsXAxisSorted zip Range(0, pointsXAxisSorted.length) foreach { case (curRowCoord,i) =>
      fullyEvalDistanceMatrix :+  (pointsXAxisSorted map {secondPt => distance(curRowCoord, secondPt)})
    }*/
    //eval all triangles
    if (evalPoints.length < 3) return None
    val allTriangles = PermutationCombination.getAllCombinations(3, Range(0, evalPoints.size).toSet)
    Option (allTriangles map { set ⇒
      perimeterofTriangle(set, evalPoints)
    } min)
  }

}

class ProblemBTesting extends FunSuite {
  def generateRandomIntegers(n : Int) : Array[Int] = {
    Random.setSeed(System.nanoTime())
    Range(0, n).toArray map { _ ⇒ Random.nextInt() % 1000 }
  }

  def generateRandomPoints(n : Int) : Array[Coordinate] = { generateRandomIntegers(n) zip generateRandomIntegers(n) map { case (x, y) ⇒ Coordinate(x, y) } }

  test ("a") {
    val pts = generateRandomPoints(100)
    ProblemB(pts)
  }
}

