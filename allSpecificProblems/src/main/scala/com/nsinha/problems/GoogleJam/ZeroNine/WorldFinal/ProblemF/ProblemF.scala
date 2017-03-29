package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemF

import com.nsinha.common._

import scala.math._
import com.nsinha.common.CoordinateDouble._
import org.scalatest.FunSuite

/** Created by nsinha on 3/27/17.
  */

case class ProblemF(inputs : String, M : Int = 2000) {

  val d : Double = 100.0 / M
  val singleBlockArea = d * d
  val (red : CoordinateDouble, green : CoordinateDouble, pillars : Map[CoordinateDouble, Double]) = processInputs
  //the x axis is right pos and y axis is bottom pos
  //val g = Grid(M, M)

  var blackCnt = 0
  var redCnt = 0
  var greenCnt = 0
  var yellowCnt = 0
  val areaPillars = findPillarsArea
  solve

  def findPillarsArea : Double = {
    pillars.foldLeft(0.0) { (Z, el) ⇒
      el._2 * el._2 * math.Pi * d * d + Z
    }
  }

  def getCenterOfBlock(blk : Coordinate) : CoordinateDouble = { CoordinateDouble(blk.x + 0.5, (M - 1 - blk.y) + 0.5) }
  def getCenterOfBlockAtOffset(blk : Coordinate, offset : Double = 0.001) : CoordinateDouble = {
    CoordinateDouble(blk.x + 0.5 + offset, (M - 1 - blk.y) + 0.5 + offset)
  }

  def getLine(c : CoordinateDouble, light : CoordinateDouble) : Line = {
    val slope = (light.y - c.y) / (light.x - c.x)
    val intercept = (c.y * light.x - c.x * light.y) / (light.x - c.x)
    Line(slope, intercept)
  }

  def doesPillarIntersectThisLineDebug(r : Double, pos : CoordinateDouble, l : Line) : Boolean = {
    //at x of pillar find y  if l calling it yl
    // if yl is less than r it does intersect
    //else not
    val normalLine = l.findNormalLinePassingThroughPoint(pos)
    val intersectionPointOption = l.findIntersectionPointOfTwoLines(normalLine)
    intersectionPointOption match {
      case None ⇒ false
      case Some(ipPoint) ⇒
        //println(ipPoint)
        val dist = distance(pos, ipPoint)
        if (FuzzyComparison.compare(abs(r), abs(dist)) >= 0) {
          true
        }
        else {
          false
        }
    }
  }

  def doesPillarIntersectThisLine(r : Double, pos : CoordinateDouble, l : Line, blockCenter : CoordinateDouble, lightPoint : CoordinateDouble) : Boolean = {
    //at x of pillar find y  if l calling it yl
    // if yl is less than r it does intersect
    //else not
    val normalLine = l.findNormalLinePassingThroughPoint(pos)
    val intersectionPointOption = l.findIntersectionPointOfTwoLines(normalLine)
    intersectionPointOption match {
      case None ⇒ false
      case Some(ipPoint) ⇒
        val dist = distance(pos, ipPoint)
        val comp = if (FuzzyComparison.compare(abs(r), abs(dist)) >= 0) true else false
        val distBlkLight = distance(blockCenter, lightPoint)
        val distBlkIp = distance(lightPoint, ipPoint)
        if (FuzzyComparison.compare(distBlkIp, distBlkLight) > 0) {
          false
        }
        else {
          comp
        }

    }

  }

  def solve = {
    val allBlocks = { Range(0, M) zip Range(0, M) } map { el ⇒ Coordinate(el._1, el._2) } toList

    for (x ← Range(0, M)) {
      for (y ← Range(0, M)) {

        val blk = Coordinate(x, y)
        val center = getCenterOfBlock(blk)
        val redLineToBlock = getLine(center, red)
        val greenLineToBlock = getLine(center, green)

        val doesIntersectRed = pillars.foldLeft(false) { (Z, entry) ⇒
          val pos = entry._1
          val rad = entry._2
          val intersect = doesPillarIntersectThisLine(rad, pos, redLineToBlock, center, red)
          if (intersect) {
            true
          }
          else {
            Z
          }
        }

        val doesIntersectGreen = pillars.foldLeft(false) { (Z, entry) ⇒
          val pos = entry._1
          val rad = entry._2
          val intersect = doesPillarIntersectThisLine(rad, pos, greenLineToBlock, center, green)
          if (intersect) {
            true
          }
          else {
            Z
          }
        }

        val insideADisc = pillars.foldLeft(false) { (Z, entry) ⇒
          val pos = entry._1
          val rad = entry._2
          val dist = distance(pos, center)
          if (FuzzyComparison.compare(rad, dist) >= 0) {
            true
          }
          else {
            Z
          }
        }

        if (!insideADisc) {
          (doesIntersectGreen, doesIntersectRed) match {
            case (true, true) ⇒
              //print(s"blk : $x $y ")
              blackCnt = blackCnt + 1
            case (true, false)  ⇒ greenCnt = greenCnt + 1
            case (false, false) ⇒ yellowCnt = yellowCnt + 1
            case (false, true)  ⇒ redCnt = redCnt + 1
          }
        }
        else {

        }
      }

    }

    println(s"pillars: ${areaPillars}")
    println(s"Black: ${blackCnt * singleBlockArea}")
    println(s"Red: ${redCnt * singleBlockArea}")
    println(s"Green: ${greenCnt * singleBlockArea}")
    println(s"Yellow: ${yellowCnt * singleBlockArea}")
  }

  def processInputs() : (CoordinateDouble, CoordinateDouble, Map[CoordinateDouble, Double]) = {
    var curRow = 0
    val inps = inputs.split("\n").toList
    val red = {
      val r = inps(0).split(" ") map (_.toInt / d)
      CoordinateDouble(r(0), r(1))
    }
    val green = {
      val r = inps(1).split(" ") map (_.toInt / d)
      CoordinateDouble(r(0), r(1))
    }

    val pillars = inps drop (3) map {
      str ⇒
        val allNos = str.split(" ") map (_.toInt / d)
        (CoordinateDouble(allNos(0), allNos(1)), allNos(2).toDouble)
    } toMap

    (red, green, pillars)
  }

}

class Testing extends FunSuite {

  test ("a") {
    val prob = ProblemF(
      """5 50
        |95 50
        |1
        |50 50 10""".stripMargin
    )
  }

  test ("b") {
    val prob = ProblemF(
      """78 71
        |74 1
        |47
        |67 8 3
        |53 90 1
        |13 12 3
        |76 77 1
        |96 39 1
        |89 69 3
        |49 96 1
        |3 88 1
        |91 52 6
        |44 4 3
        |11 85 6
        |97 71 1
        |74 85 7
        |51 7 2
        |97 57 1
        |2 71 1
        |84 82 1
        |81 76 3
        |42 50 40
        |83 14 12
        |69 13 1
        |34 96 2
        |42 97 1
        |22 2 1
        |93 91 4
        |37 6 4
        |60 7 1
        |6 19 4
        |13 95 2
        |86 79 2
        |96 62 1
        |98 3 1
        |5 27 3
        |95 6 2
        |90 75 2
        |76 97 2
        |3 13 1
        |7 95 3
        |28 92 3
        |90 36 5
        |86 95 4
        |5 72 2
        |96 24 3
        |53 94 1
        |62 12 1
        |21 87 1
        |90 28 1""".stripMargin
    )
  }
}
