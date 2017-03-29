package com.nsinha.common

import math._

/** Created by nsinha on 3/28/17.
  *
  *
  */

case class CoordinateDouble(x : Double, y : Double)
object CoordinateDouble {
  def distance(p : CoordinateDouble, q : CoordinateDouble) : Double = {
    val disSq = pow((p.x - q.x), 2) + pow(p.y - q.y, 2)
    sqrt(disSq)
  }

  implicit def getCoordinateDoubleFromCoordinate(c : Coordinate) : CoordinateDouble = {
    CoordinateDouble(c.x, c.y)
  }
}

object FuzzyComparison {

  def compare(x : Double, y : Double, afterDecimals : Int = 5) : Int = {
    val xRaised : Int = { x * math.pow(10.0, afterDecimals) }.toInt
    val yRaised : Int = { y * math.pow(10.0, afterDecimals) }.toInt
    if (xRaised == yRaised) {
      0
    }
    else if (xRaised > yRaised) {
      1
    }
    else {
      -1
    }
  }
}

case class Line(slope : Double, intercept : Double) {

  def getAPointOnLineXGiven(x : Double) : CoordinateDouble = { CoordinateDouble(x, slope * x + intercept) }
  def getAPointOnLineYGiven(y : Double) : CoordinateDouble = { CoordinateDouble((y - intercept) / slope, y) }

  def findIntersectionPointOfTwoLines(m : Line) : Option[CoordinateDouble] = {
    //sanity test and early return for degenerate cases
    if (FuzzyComparison.compare(slope, m.slope, 4) == 0) {
      return None
    }
    val diffSlopes = slope - m.slope
    val diffIntercepts = intercept - m.intercept
    val intersectX = -diffIntercepts / diffSlopes
    val intersectPoint = getAPointOnLineXGiven(intersectX)

    Option(intersectPoint)
  }

  def findNormalLinePassingThroughPoint(p : CoordinateDouble) : Line = {
    val slopeNormal = -1.0 / slope
    val intercept = p.y - slopeNormal * p.x
    Line(slopeNormal, intercept)
  }

}
