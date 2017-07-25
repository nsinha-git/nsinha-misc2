package com.nsinha.problems.RecursionAndMaryTrees

import org.scalatest.FunSuite
import sun.java2d.loops.TransformHelper

import scala.collection.immutable.TreeMap
import scala.collection.mutable

/** Created by nsinha on 6/12/17.
  */
class RecursiveSim {

}

case object PartitionOfIntegerMaryTree {

  def findAllPartitions(n : Int) : Set[List[Int]] = {

    /* let n = 10
    we can do this: 9 , list(1)
                    8,list(2)
                    7,list(3)...
                    0,list(9)

    no consider 9 ,list(1):
                    8, list(1,1)
                    7 list(1,2)
                    6 list(1,3)..
                    0 list(1,9)
   now consider 8,list(1,1):
                    7, list(1,1,1)
                    0, list(1,1,,8)
   base case is: 0, list(..) => list(..)

    is this a set?
    it's a set but there but a permutation set.
     */

    findAllPartitions2(n, Set(List(0)))
  }

  def findAllPartitions2(n : Int, l : Set[List[Int]]) : Set[List[Int]] = {
    if (n == 0) {
      return l
    }

    val resList = mutable.Set[List[Int]]()

    for (i ← Range(1, n + 1)) {
      val remainder = n - i
      val ll = l map { list ⇒ list :+ i sorted }
      findAllPartitions2(remainder, ll) foreach {
        el ⇒ resList += el
      }
    }

    return resList.toSet
  }

}

case object PermutationAsRecursionMAryTree {

  def findAllPerms(nos : Int) : List[Map[Int, Int]] = {
    /* A1. if place last digit at all the n cols
    A2.For all the list place the last digit everywhere if not filled.
     */
    val baseList = createBaseList(nos)
    nAryPerm(baseList, nos - 1, nos)
  }

  def createBaseList(nos : Int) : List[Map[Int, Int]] = {
    val res = for (i ← Range(0, nos)) yield {
      TreeMap(i → nos)
    }
    res toList
  }

  def nAryPerm(ll : List[Map[Int, Int]], nos : Int, total : Int) : List[Map[Int, Int]] = {
    if (nos == 0) return ll
    val curNo = nos
    var Z = mutable.MutableList[Map[Int, Int]]()
    ll foreach { el ⇒
      for (j ← Range(0, total)) {
        if (!el.contains(j)) {
          Z += el.+ ((j → curNo))
        }
      }
    }
    nAryPerm(Z.toList, nos - 1, total)
  }

}

class MaryTesting extends FunSuite {
  import PermutationAsRecursionMAryTree._
  test ("a") {
    val x = findAllPerms(5)
    println(x)
  }

  test("b") {
    import PartitionOfIntegerMaryTree._
    for (i ← Range(1, 100)) {
      val x = findAllPartitions(i)
      println(i+" "+x.size+" ")
    }

  }
}
