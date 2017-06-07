package com.nsinha.problems.GoogleJam.Thirteen.WorldFinal.ProblemA

/** Created by nsinha on 5/17/17.
  */

case object Local {
  type Time = Long
  type Intersects = Long
  type Cars = Int
  type Length = Long

}
import Local._
case class ProblemA(T : Time, C : Cars, N : Intersects, Cars : Array[(Time, Intersects, Time)]) {
  def getNextInter(x : Intersects) : Intersects = {
    getCCWInter(x, 1)
  }

  def getPrevInter(x : Intersects) : Intersects = {
    getCWInter(x, 1)
  }

  def getCCWInter(x : Intersects, d : Long) : Intersects = {
    (x + d) % N
  }

  def getCWInter(x : Intersects, d : Long) : Intersects = {
    (x - d) % N
  }

  def findGoodPoints : List[(Time, Intersects)] = {
    Cars.toList flatMap { x ⇒ List((x._1, getNextInter(x._2)), (x._1 + x._3, getPrevInter(getCCWInter(x._2, x._3))), (x._1 + x._3 / 2 - 1, getCCWInter(x._2, x._3 / 2)), (x._1 + x._3 / 2 + 1, getCCWInter(x._2, x._3 / 2))) }
  }

  def findLeftLength(p : (Time, Intersects), q : (Time, Intersects, Time)) : Option[Length] = {
    //(t+ ts)/2 - (m-n)/2.
    val ti = (p._1 + q._1) / 2 - getCCWInter(q._2, p._2)
    val tiMts = ti - q._1

    if (tiMts >= 0 && tiMts <= q._3) {
      Option(p._1 - ti - 1)
    }
    else {
      None
    }
  }

  def findRightLength(p : (Time, Intersects), q : (Time, Intersects, Time)) : Option[Length] = {
    //ti as (t+ts)/2 - (n-m)/2.
    val ti = (p._1 + q._1) / 2 - getCWInter(q._2, p._2)
    val tiMts = ti - q._1

    if (tiMts >= 0 && tiMts <= q._3) {
      Option(p._1 - ti - 1)
    }
    else {
      None
    }
  }

  def processASingleGoodPoint(p : (Time, Intersects)) : Length = {
    var minLeftLen = T
    var minRightLen = T
    Cars.toList foreach { car ⇒
      if (car._1 < p._1) {
        val curLeftLen = findLeftLength(p, car)
        Option(curLeftLen) map {
          op ⇒
            op match {
              case None ⇒
              case Some(unOpted) ⇒
                if (minLeftLen > unOpted) minLeftLen = unOpted
            }
        }
      }
      else if (car._1 > p._1) {
        val curRightLen = findLeftLength(p, car)
        Option(curRightLen) map {
          op ⇒
            op match {
              case None ⇒
              case Some(unOpted) ⇒
                if (minRightLen > unOpted) minRightLen = unOpted
            }
        }
      }
      else {
        val curLeftLen = findLeftLength(p, car)
        Option(curLeftLen) map {
          op ⇒
            op match {
              case None ⇒
              case Some(unOpted) ⇒
                if (minLeftLen > unOpted) minLeftLen = unOpted
            }
        }
        val curRightLen = findLeftLength(p, car)
        Option(curRightLen) map {
          op ⇒
            op match {
              case None ⇒
              case Some(unOpted) ⇒
                if (minRightLen > unOpted) minRightLen = unOpted
            }
        }
      }
    }

    minLeftLen + minRightLen

  }

  def processAllGoodPoints : Length = {
    val goodPoints = findGoodPoints
    var maxLen = 0L

    goodPoints foreach { p ⇒
      val curPLen = processASingleGoodPoint(p)
      if (maxLen < curPLen) maxLen = curPLen
    }
    maxLen
  }

}
