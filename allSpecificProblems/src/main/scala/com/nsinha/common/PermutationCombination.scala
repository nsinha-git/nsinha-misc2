package com.nsinha.common

import org.scalatest.FunSuite

import scala.collection.immutable.TreeSet
import scala.collection.mutable

/** Created by nsinha on 4/12/17.
  */
object PermutationCombination {

  def getAllPerms(elems : List[Int]) : (Int, List[List[Int]]) = {
    var res = mutable.MutableList[List[Int]]()
    elems foreach { el ⇒ res = placeInsidePerm(el, res) }

    (res.length, res.toList)
  }

  private def placeInsidePerm(x : Int, res : mutable.MutableList[List[Int]]) : mutable.MutableList[List[Int]] = {
    val res1 = mutable.MutableList[List[Int]]()

    if (res.nonEmpty) {
      val len = res.head.length
      Range(0, len + 1) foreach { i ⇒
        res foreach { list ⇒
          res1 += (list.slice(0, i) :+ x) ++ list.slice(i, len)
        }
      }
    }
    else {
      res1 += List(x)
    }

    res1
  }

  def factorial(x : Int) : Long = {
    if (x <= 1) return 1
    x * factorial(x - 1)
  }

  def factorial(x : Int, y : Int, z : Int) : Long = {
    if (x <= 1) return 1

    if (x <= y || x <= z) return 1

    x * factorial(x - 1, y, z)
  }

  def nchoosek(n : Int, k : Int) : Long = {
    if (k <= 0 || n <= 0) return 1
    factorial(n, k, n - k) / factorial(math.min(k, n - k))
  }

  def getNextPerm(elems : List[Int]) : List[Int] = {
    val ordered = elems.sortBy(x ⇒ -x)
    val indexOfThisPerm = ordered.foldLeft(0L, elems) { (Z, el) ⇒
      val mapOfElToIndex = { Z._2.reverse zip Range(0, Z._2.length) map { case (ell, i) ⇒ ell → i } }.toMap
      (Z._1 + mapOfElToIndex(el) * factorial(Z._2.length - 1), Z._2 filter (k ⇒ k != el))
    }._1

    val bitSet = Range(0, elems.length).foldLeft(mutable.BitSet()){ (Z, el) ⇒ Z.+=(el) }
    var indexOfNextPerm = (indexOfThisPerm + 1) % factorial(elems.length)

    var dividend = indexOfNextPerm
    val positionMap = ordered map { x ⇒
      val quotient = dividend / factorial(bitSet.size - 1)
      dividend = dividend - quotient * factorial(bitSet.size - 1)
      val index = findBitSetPos(bitSet, quotient)
      bitSet.remove(index)
      x → index
    } toMap

    createANumber(positionMap)
  }

  private def findBitSetPos(bitSet : mutable.BitSet, r : Long) : Int = {
    var res = 0
    var cnt = 0
    bitSet foreach { el ⇒
      if (cnt == r) res = el
      cnt = cnt + 1
    }
    res
  }

  private def createANumber(mp : Map[Int, Int]) : List[Int] = { mp.toList.sortBy(_._2) map (_._1) reverse }

  def getAllCombinations(part : Int, total : Set[Int]) : Set[Set[Int]] = {
    val res = mutable.MutableList[Int]()

    if (part == 1) return {
      total map { x ⇒ TreeSet(x) }
    }

    if (part == 0) return Set()

    val oneLessPart = getAllCombinations(part - 1, total)

    total flatMap { el ⇒
      oneLessPart map { x ⇒
        if (x.min < el) {
          x + el
        }
        else {
          Set[Int]()
        }
      }
    } filter (el ⇒ el.nonEmpty && el.size == part)
  }

}

class PermutationCombinationTesting extends FunSuite {
  test("a") {
    val res = PermutationCombination.getAllPerms(List(1, 2, 3))
    println(res)
  }
  test("b") {
    var initialList = List(1, 2, 3, 4)
    Range(0, 24) foreach { x ⇒
      initialList = PermutationCombination.getNextPerm(initialList)
      println(initialList)
    }
  }
  test("c") {
    var initialList = Set(1, 2, 3, 4)
    println(PermutationCombination.getAllCombinations(3, initialList))
  }
}
