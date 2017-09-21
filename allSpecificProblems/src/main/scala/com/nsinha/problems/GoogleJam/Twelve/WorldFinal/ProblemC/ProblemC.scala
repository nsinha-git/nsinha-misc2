package com.nsinha.problems.GoogleJam.Twelve.WorldFinal.ProblemC

import org.scalatest.FunSuite

import scala.collection.mutable
import math._

/**
  * Created by nishchaysinha on 8/24/17.
  */

trait Shell
object CShell extends Shell
object HShell extends Shell

class ProblemC {

  def solve(in: List[(Int, Int, Shell)]): (Int, Int) = {
    val res = findMinimum (solveList(in))
    if (res == null) return null
    else res
  }

  implicit val ord = new Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = {
      val d1 = x._1* x._1 + x._2*x._2
      val d2 = y._1* y._1 + y._2*y._2
      if (d1 < d2) {
        return 1
      } else if (d1 > d2) return -1

      //d1 ==d2 return with greater x

      if(x._1 < y._1) {
        return -1
      } else if (x._1 > y._1) {
        return 1
      } else {
        if(x._2 < y._2) {
          return -1
        } else if (x._2 > y._2) {
          return 1
        } else {
          return 0
        }
      }
    }
  }

  def findMinimum(value: List[(Int, Int)]): (Int, Int) = {
    if (value.nonEmpty) {
      val sortedOnXY = value.sorted
      sortedOnXY.head
    }
    else {
      null
    }
  }


  def solveList(in: List[(Int, Int, Shell)]): List[(Int, Int)] = {
    val res = mutable.MutableList[(Int, Int)]()

    for (i <- Range(0, 1000)) {
      for (j <- Range(0, 1000)) {
        val center = (i,j)
        if (checkCenter(center, in)) {
          res += center
        }
      }
    }
    res.toList
  }


  def checkCenter(center: (Int, Int), in: List[(Int, Int, Shell)]): Boolean = {
    for (p <- in) {
      val isSatisfied = if (p._3 == HShell) {
        checkCenterWithHShell(center, (p._1,p._2))
      } else {
        checkCenterWithCShell(center, (p._1,p._2))
      }

      if (isSatisfied == false) return false

    }
    return true
  }

  def checkCenterWithHShell(center: (Int, Int), point: (Int, Int)): Boolean = {

    val pointTransformed = (abs(point._1 - center._1), abs(point._2 - center._2))

    val maxval = max (pointTransformed._1, pointTransformed._2)

    if ( maxval % 2 == 0  ) {
      false
    } else {
      true
    }
  }


  def checkCenterWithCShell(center: (Int, Int), point: (Int, Int)): Boolean = {
    !checkCenterWithHShell(center, point)
  }

}

class TestingProblemC extends FunSuite {

  test("1") {
    val in = List ((50,30, HShell), (49,30, HShell), (49,31, HShell),(49,32, HShell),(50,32, HShell))
    val pr = new ProblemC
    print (pr.solve(in))
  }

  test ("2") {


  }


}
