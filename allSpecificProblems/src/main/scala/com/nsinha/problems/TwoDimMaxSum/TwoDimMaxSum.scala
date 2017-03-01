package com.nsinha.problems.TwoDimMaxSum

import com.sun.deploy.util.SyncFileAccess.RandomAccessFileLock

import scala.collection.mutable
import org.scalatest.{FunSuite, MustMatchers}

import scala.util.Random

/** Created by nsinha on 2/28/17.
  */

//Coordinate

case class Coordinate(x : Int, y : Int)

class TwoDimMaxSum(left : Coordinate, right : Coordinate, values : (Int, Int) ⇒ Int, Mopt : Option[Map[Coordinate, Int]] = None,
                   dpTable : mutable.Map[(Coordinate, Coordinate), Int] = mutable.Map[(Coordinate, Coordinate), Int](),
                   maxMap :  mutable.Map[Int, (Coordinate, Coordinate)] = mutable.Map[Int, (Coordinate, Coordinate)]()) {

  val (leftTop, rightBottom) = getLeftTopRightBottom(left, right).get

  val M : Map[Coordinate, Int] = Mopt match {
    case None    ⇒ initMatrix()
    case Some(m) ⇒ m
  }

  val (rows, cols) = (rightBottom.x - leftTop.x, rightBottom.y - leftTop.y)

  def initMatrix() : Map[Coordinate, Int] = {
    val mp = mutable.Map[Coordinate, Int] ()
    for {
      row ← Range(leftTop.x, rightBottom.x)
      col ← Range(leftTop.y, rightBottom.y)
    } {
      mp += (Coordinate(row, col) → values(row, col))
    }
    mp map {
      el ⇒
        val left = el._1
        val right = Coordinate(left.x + 1, left.y + 1)
        dpTable += ((left, right) → el._2)
        maxMap.keys.headOption match {
          case None ⇒ maxMap += el._2 → (left, right)
          case Some(maxTerm) ⇒ if (maxTerm < el._2) {
            maxMap.clear()
            maxMap += el._2 → (left, right)
          }
        }
    }

    mp.toMap
  }
  /*
  we have choosen were points separation represent matrix dimensions. In this represenation,
  given p1 p2 this fn tries to find the minimum matrix that can contain these two cooridinates
  if p1 and p2 are same we can not create a matrix because the rep we chose needs two points for a valid length in x/y or both.
  . A single point has no dimensions and no matrix can exist.
  A boundry check happens when the containing matrix goes over boundry.
   */
  def getLeftTopRightBottom(p1 : Coordinate, p2 : Coordinate) : Option[(Coordinate, Coordinate)] = {
    val minX = math.min(p1.x, p2.x)
    val maxX = math.max(p1.x, p2.x)
    val minY = math.min(p1.y, p2.y)
    val maxY = math.max(p1.y, p2.y)
    val lenX = maxX - minX
    val lenY = maxY - minY

    val (maxXXOpt, maxYYOpt) = (lenX, lenY) match {
      case (0, 0)         ⇒ (None, None)
      case (lenXX, 0)     ⇒ (Some(maxX), Some(minY + 1))
      case (0, lenYY)     ⇒ (Some(minX + 1), Some(maxY))
      case (lenXX, lenYY) ⇒ (Some(maxX), Some(maxY))
    }
    maxXXOpt match {
      case None ⇒ None
      case Some(maxXX) ⇒
        if (rightBottom != null && (maxXX > rightBottom.x || maxYYOpt.get > rightBottom.y)) {
          None
        }
        else {
          Some(Coordinate(minX, minY), Coordinate(maxXX, maxYYOpt.get))
        }
    }
  }

  def maxsum() : (Int, Coordinate, Coordinate) = {
    for {
      row1 ← Range(leftTop.x, rightBottom.x + 1)
      row2 ← Range(leftTop.x, rightBottom.x + 1)
      col1 ← Range(leftTop.y, rightBottom.y + 1)
      col2 ← Range(leftTop.y, rightBottom.y + 1)
    } {
      val normalizedCoordinatesOpt = getLeftTopRightBottom(Coordinate(row1, col1), Coordinate(row2, col2))
      normalizedCoordinatesOpt match {
        case None ⇒
        case Some(normalizedCoordinates) ⇒
          val (subMatrixLT, subMatrixRB) = normalizedCoordinates
          if (!dpTable.contains((subMatrixLT, subMatrixRB)) && !(subMatrixLT == leftTop && subMatrixRB == rightBottom)) {
            println(subMatrixLT)
            println(subMatrixRB)
            val newProblem = new TwoDimMaxSum(subMatrixLT, subMatrixRB, values, Some(M), dpTable, maxMap)
            newProblem.maxsum()
          }
      }
    }

    println(s"computing sum for $leftTop $rightBottom")

    val thisSum = if (cols > 1) {
      //divide the matrix in two parts one with width 1 and other with remaining
      val t1 = Coordinate(rightBottom.x, leftTop.y + 1)
      val t2 = Coordinate(leftTop.x, leftTop.y + 1)
      println(s"${(leftTop, t1)} ${(t2, rightBottom)}")
      val sum = dpTable((leftTop, t1)) + dpTable((t2, rightBottom))
      dpTable((leftTop, rightBottom)) = sum
      println(s"sum = $sum")
      sum
    }
    else if (rows > 1) {
      val t1 = Coordinate(rightBottom.x, leftTop.y + 1)
      val t2 = Coordinate(leftTop.x, leftTop.y + 1)
      println(s"${(leftTop, t1)} ${(t2, rightBottom)}")
      val sum = dpTable((leftTop, Coordinate(leftTop.x + 1, rightBottom.y))) + dpTable((Coordinate(leftTop.x + 1, leftTop.y), rightBottom))
      dpTable((leftTop, rightBottom)) = sum
      println(s"sum = $sum")
      sum
    }
    else {
      println(s"${(leftTop, rightBottom)}")
      val sum = dpTable(leftTop, rightBottom)
      println(s"sum = $sum")
      sum
    }

    if (maxMap.head._1 < thisSum) {
      maxMap.clear()
      maxMap += (thisSum → (leftTop, rightBottom))
    }
    (maxMap.head._1, maxMap.head._2._1, maxMap.head._2._2)
  }

  implicit def sortingFn : Ordering[Coordinate] = {
    new Ordering[Coordinate] {
      override def compare(x : Coordinate, y : Coordinate) = {
        if (x.x > y.x) {
          1
        }
        else if (x.x == y.y) {
          if (x.y > y.y) {
            1
          }
          else if (x.y == y.y) {
            0
          }
          else {
            -1
          }
        }
        else {
          -1
        }
      }
    }
  }

  def printM = {
    var changeRow = 0
    for (key ← M.keys.toList.sorted) {
      val value = M(key)
      if (changeRow != key.x) {
        println()
        changeRow = key.x
      }
      print(value+" ")
    }
  }

}

class Testing extends FunSuite {
  test("3x3") {
    val soln = new TwoDimMaxSum(Coordinate(0, 0), Coordinate(2, 2), (x : Int, y : Int) ⇒ {
      val x = { new Random() }.nextInt() % 10
      val s = { new Random() }.nextBoolean()
      if (s) x else -x
    })
    soln.printM
    println()
    println(soln.maxsum())
    println()
  }

}
