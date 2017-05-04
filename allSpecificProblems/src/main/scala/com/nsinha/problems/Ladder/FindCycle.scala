package com.nsinha.problems.Ladder

import com.nsinha.common.PermutationCombination

import scala.annotation.tailrec
import scala.collection.mutable

/** Created by nsinha on 4/17/17.
  */

case class FindCycle(g : Map[Node, List[Node]]) {
  /* at max o(2e+n) */
  def isValid : Boolean = {
    val mostSeniorAncestorSet = findMostSeniorAncestors //o(e)
    mostSeniorAncestorSet foreach { head ⇒ //every head is orthogonal
      val res = checkCycles(head)
      if (!res) return false
    } //total is o(e+n)

    return true
  }

  def checkCycles(curNode : Node) : Boolean = {
    val enQuedAlreadyOnThisIter = mutable.HashSet[Node](curNode)
    val res = doBfsWithAggregatedAncestorQueAndCurrentLevelQue(mutable.Queue[Node](curNode), enQuedAlreadyOnThisIter)
    if (res == false) return false
    return true
  }

  def doBfsWithAggregatedAncestorQueAndCurrentLevelQue(bfsQue : mutable.Queue[Node], enQuedAlready : mutable.Set[Node]) : Boolean = {
    var curQue = bfsQue //curBfslevel to be read
    var nextQueSet = mutable.HashSet[Node]() //nextbfslevel to be written

    while (curQue.nonEmpty && nextQueSet.nonEmpty) {
      val tempQueSet = curQue.toSet
      while (curQue.nonEmpty) {
        val curNode = curQue.dequeue()
        val nbrs = g(curNode)
        nbrs foreach { nbr ⇒
          if (enQuedAlready.contains(nbr)) return false
          if (!tempQueSet.contains(nbr)) nextQueSet += nbr
        }
      }
      //curQue is turned empty
      if (nextQueSet.nonEmpty) {
        nextQueSet foreach { el ⇒
          curQue += el
          enQuedAlready += el
        }
      }
      nextQueSet.clear()
    }
    return true
  }

  def findMostSeniorAncestors : mutable.Set[Node] = { //o(e)
    val nodeWithParentsSet = mutable.Set[Node]()
    g foreach {
      case (node, nbrs) ⇒
        nbrs foreach (nbr ⇒ nodeWithParentsSet += nbr)
    }
    val setOfNoParentsNodes = g.keySet.diff(nodeWithParentsSet)
    setOfNoParentsNodes.foldLeft(mutable.Set[Node]()) { (Z, el) ⇒ Z += el }
  }
}

object Binomial {
  @tailrec
  def factorial(n : Int, acc : BigDecimal = 1) : BigDecimal = {
    if (n <= 1) return acc
    factorial(n - 1, acc * n)
  }

  def generatenck(n : Int, k : Int) : BigDecimal = {
    val num = factorial(n)
    val denum = factorial(k) * factorial(n - k)
    val z = num / denum
    z
  }

  def arbitDist(n : Int) = {
    var Z = BigDecimal(0)
    for (i ← Range(0, n + 1)) {
      Z = Z + generatenck(n, i) * 2 * math.min(i, n - i)
    }
  }

  def findLISLen(l : List[Int]) : Int = { 0 }

  def main(args : Array[String]) : Unit = {

    for (nn ← Range(1, 8)) {
      val n = math.pow(10, nn).toInt
      val fact = factorial(n)
      val nbye = BigDecimal(n / math.E).pow(n)
      print(n+":")
      println(fact / (nbye * math.sqrt(n)))
    }
  }

  def main3(args : Array[String]) : Unit = {

    val p = 102400
    var z = BigDecimal(0.0)
    for (i ← Range(1, p)) {
      z = z + BigDecimal(1) / (BigDecimal(i) * BigDecimal(i + 1))
    }

    println(z * BigDecimal(p))

  }

  def main2(args : Array[String]) : Unit = {
    val N = 100
    for (n ← Range(0, N)) {
      println(n+":"+generatenck(n, n / 2) / BigDecimal(2).pow(n))
    }

  }

  def main1(args : Array[String]) : Unit = {
    val N = 100
    for (n ← Range(0, N)) {
      var z = BigDecimal(0)
      for (i ← Range(0, n + 1)) {
        val x = generatenck(n, i)
        z = z + (x * math.abs(n - 2 * i))
      }
      print(n+" :")
      println(z / BigDecimal(2).pow(n))
    }

    for (n ← Range(0, N)) {
      var z = BigDecimal(0)
      for (i ← Range(0, n + 1)) {
        val x = generatenck(n, i)
        z = z + x * 2 * i
      }
      print(n+" :")
      println(z / BigDecimal(2).pow(n))
    }

    for (n ← Range(0, N)) {
      var z = BigDecimal(0)
      for (i ← Range(0, n + 1)) {
        val x = generatenck(n, i)
        z = z + x * n
      }
      print(n+" :")
      println(z / BigDecimal(2).pow(n))
    }

    for (n ← Range(0, N)) {
      var z = BigDecimal(0)
      for (i ← Range(math.max(n / 3, 0), n / 2)) {
        val x = generatenck(n, i)
        z = z + x * ((n - 2 * i))
      }
      print(n+" :")
      println(z / BigDecimal(2).pow(n))
    }
  }

}
