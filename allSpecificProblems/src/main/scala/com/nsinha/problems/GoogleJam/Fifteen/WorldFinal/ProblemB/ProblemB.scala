package com.nsinha.problems.GoogleJam.Fifteen.WorldFinal.ProblemB

import com.nsinha.common.PermutationCombination._
import org.scalatest.FunSuite

import scala.collection.immutable.TreeMap
import scala.collection.mutable

/** Created by nsinha on 5/28/17.
  */
case class ProblemB(n : Int, x : Int) {

  def solve : Long = {
    var sum = 0L

    for (exactX ← Range(x, n + 1)) {
      sum = sum + solveWithExactX(exactX)
    }
    sum

  }

  def solveWithExactX(curX : Int) : Long = {
    //choose curX rows and curX cols in nchoosecurX ^2
    val temp = nchoosek(n, curX)
    val totalCombs = temp * temp * factorial(curX)
    //now we need to fill 1,2  the n-curx rows and n-curx cols
    val all12fills = fill12(n - curX, n - curX, 0, 0)
    totalCombs * all12fills
  }

  def fill12(numRows : Int, numCols : Int, numOf1FilledCols : Int, numOf2FilledCols : Int) : Long = {
    //choose any row
    import com.nsinha.common.PermutationCombination._
    val totalFreecols = numCols - numOf1FilledCols - numOf2FilledCols
    if (numCols == 0 && numOf1FilledCols == 0 && numOf2FilledCols == 0) return 1l
    if (numCols < 0 || numOf1FilledCols < 0 || numOf2FilledCols < 0) return 0l

    { if (totalFreecols > 0) nchoosek(totalFreecols, 2) * 2 * fill12(numRows - 1, numCols - 2, numOf1FilledCols + 1, numOf2FilledCols + 1) else 0 } +
      { if (numCols > 0 & numOf1FilledCols > 0) numCols * numOf1FilledCols * fill12(numRows - 1, numCols - 1, numOf1FilledCols - 1, numOf2FilledCols + 1) else 0 } +
      { if (numCols > 0 & numOf2FilledCols > 0) numCols * numOf2FilledCols * fill12(numRows - 1, numCols - 1, numOf1FilledCols + 1, numOf2FilledCols - 1) else 0 } +
      { if (numOf1FilledCols > 0 & numOf2FilledCols > 0) numOf1FilledCols * numOf2FilledCols * fill12(numRows - 1, numCols, numOf1FilledCols - 1, numOf2FilledCols - 1) else 0 }
  }
}

class ProblemBTesting extends FunSuite {

  test("a") {
    val res = ProblemB(3, 1).solve
    print(res)
  }

}

object TestMean extends App {
  run

  def run = {
    for (cnt ← Range(6, 100)) {

      val objects = Range(1, cnt + 1) toList
      val mpFreq = findFrequency3(objects)

      val total = (mpFreq.foldLeft(0) { (Z, el) ⇒ Z + el._2 })
      val totalTriples = mpFreq.size
      println(s"cnt = ${cnt - 1} total =$total totalTriples = $totalTriples freq = ${total * 1.0 / (6 * totalTriples * nchoosek(cnt - 4, 3))}")
    }
  }

  def getAllPairs(objects : List[Int]) = {
    val mList = mutable.MutableList[(Int, Int)]()

    for {
      f ← objects
      g ← objects if f != g
    } {
      mList += ((f, g))
    }
    mList toList
  }

  def getAllTriples(objects : List[Int]) = {
    val mList = mutable.MutableList[(Int, Int, Int)]()

    for {
      f ← objects
      g ← objects if f != g
      h ← objects if (h != g) && (h != f)
    } {
      mList += ((f, g, h))
    }
    mList toList
  }

  def findFrequency3(objects : List[Int]) : Map[(Int, Int, Int), Int] = {

    val l = getAllTriples(objects)

    val res = for (curTriple ← l) yield {
      var i = 0
      for (compTriple ← l) {
        if ((noSharedNumbersSize(compTriple, curTriple) == 0) && canFit(compTriple, curTriple)) {
          i = i + 1
        }
      }
      curTriple → i
    }

    res.foldLeft(TreeMap[(Int, Int, Int), Int]()) {
      (Z, el) ⇒ Z + el
    }
  }
  def findFrequency2(objects : List[Int]) : Map[(Int, Int), Int] = {

    val l = getAllPairs(objects)

    val res = for (curPair ← l) yield {
      var i = 0
      for (compPair ← l) {
        if (!noSharedNumbers(compPair, curPair) && canFit(compPair, curPair)) {
          i = i + 1
        }
      }
      curPair → i
    }

    res.foldLeft(TreeMap[(Int, Int), Int]()) {
      (Z, el) ⇒ Z + el
    }
  }

  def noSharedNumbers(x : (Int, Int), y : (Int, Int)) : Boolean = {
    (x._1 == y._1) || (x._1 == y._2) || (x._2 == y._1) || (x._2 == y._2)
  }

  def noSharedNumbersSize(x : (Int, Int, Int), y : (Int, Int, Int)) : Int = {
    val s = mutable.Set[Int]()
    val c = mutable.Set[Int]()
    s += x._1; s += x._2; s += x._3
    c += y._1; c += y._2; c += y._3
    val d = s.intersect(c)
    d.size
  }

  def canFit(x : (Int, Int), y : (Int, Int)) : Boolean = {
    (x._1 > y._1) && (x._2 > y._2)
  }

  def canFit(x : (Int, Int, Int), y : (Int, Int, Int)) : Boolean = {
    (x._1 > y._1) && (x._2 > y._2) && (x._3 > y._3)
  }

}

/*

Abhishek


 */
