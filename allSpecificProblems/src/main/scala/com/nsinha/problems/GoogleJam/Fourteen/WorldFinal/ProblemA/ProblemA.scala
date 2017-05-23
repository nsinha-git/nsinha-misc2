package com.nsinha.problems.GoogleJam.Fourteen.WorldFinal.ProblemA

import scala.collection.mutable

/** Created by nsinha on 5/17/17.
  */
case class ProblemA(size : Int, matrix : Array[Array[Boolean]]) {

  def solve : Option[Int] = {
    val (a0, a0C) = findFirstRowAndComplement

    val (s1, s2) = collectIntoSets(a0, a0C)
    if (s1.size != size / 2) return None
    if (s2.size != size / 2) return None
    val colsOpsCount = countUnParityPosInRow(a0)
    val rowsOpsCount = countUnParityPosInCol(s1)
    Option(colsOpsCount + rowsOpsCount)
  }

  def complement(arr : Array[Boolean]) : Array[Boolean] = {
    arr map (x ⇒ !x)
  }

  def matchArray(a0 : Array[Boolean], a1 : Array[Boolean]) : Boolean = {
    if (a0.length != a1.length) return false

    { a0 zip a1 map { case (x, y) ⇒ x == y } }.forall(_ == true)

  }

  def findFirstRowAndComplement : (Array[Boolean], Array[Boolean]) = {
    val arr0 = matrix(0)
    val compl = complement(arr0)
    (arr0, compl)
  }
  def collectIntoSets(a0 : Array[Boolean], a0C : Array[Boolean]) : (Set[Int], Set[Int]) = {
    val s1 = mutable.Set[Int]()
    val s2 = mutable.Set[Int]()
    matrix zip Range(0, matrix.length) foreach {
      case (arr, i) ⇒
        if (matchArray(arr, a0)) {
          s1 += i
        }
        else {
          matchArray(arr, a0C)
        }
    }
    (s1.toSet, s2.toSet)
  }

  def countUnParityPosInRow(arr : Array[Boolean]) : Int = {
    val model = arr(0)
    var cnt = 0
    arr zip Range(0, arr.length) foreach {
      case (x, i) ⇒ if (x == model && i % 2 == 1) cnt = cnt + 1
    }
    cnt
  }

  def countUnParityPosInCol(set : Set[Int]) : Int = {
    var cnt = 0
    set foreach {
      case x ⇒ if (x % 2 == 1) cnt = cnt + 1
    }
    cnt
  }

}
