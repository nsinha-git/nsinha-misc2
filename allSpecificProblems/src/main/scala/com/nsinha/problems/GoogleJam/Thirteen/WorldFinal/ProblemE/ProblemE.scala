package com.nsinha.problems.GoogleJam.Thirteen.WorldFinal.ProblemE

import org.scalatest.FunSuite

import scala.collection.mutable

/**
  * Created by nishchaysinha on 9/12/17.
  */

case class ProblemETop(inStr: String) {
  val input = inStr.split(" ") map {_.toInt}
  println(ProblemE(input).solveBrute())
}

class CommonE(input:Array[Int]) {

  var problemIndices  = mutable.HashMap[Int, Set[Int]]()

  var problemExist = false

  def generateProblemIndices(): Unit = {
    for (i <- Range(0, input.size)) {
      val curI = input(i)

      val ll = mutable.HashSet[Int]()

      for (j <- Range(i + 1, input.size)) {
        if (curI < input(j)) {
          problemExist = true
          ll += j
        }
      }
      problemIndices += i -> ll.toSet
    }
  }
}


case class ProblemE(input: Array[Int] ) {
  import ProblemE._

  val commonE = new CommonE(input)






  def solveBrute(): Long = {
    import commonE._
    if (cache.contains(input.foldLeft("") {(Z,el) => Z + el.toString})) {
      return cache(input.foldLeft("") {(Z,el) => Z + "-" +el.toString})
    }
    generateProblemIndices()

    if (!problemExist) return 1
   // if (!problemExist && lastIndicesSet.size == input.size) return factorial(input.size)

    val results = Range(0, input.size) map { i => solveThisIndexExcludedBrute(i) }

    val res = results.foldLeft(0L) {(Z,el) => Z + el}
    cache += (input.foldLeft("") {(Z,el) => Z + "-"+ el.toString}) -> res
    res
  }


  def solveThisIndexExcludedBrute(i: Int): Long = {
    import commonE._
    /* 1. Include this index
       2. Exclude any index that comes before this index for isolation (orthogonality) that will help us in not double counting
       3. For non problem indexes find all combinations of exc/include
       4. For problem indices have to remove every one of it.
      */
    val remainingArray = input.slice(0,i) ++ input.slice(i+1, input.size)//takes care of 1 and 2
    ProblemE(remainingArray).solveBrute()
  }
}


object ProblemE {

  val cache: mutable.HashMap[String, Long] = mutable.HashMap.empty

  def factorial(i: Int): Long = {
    factorialInt(i,1)
  }

  def factorialInt(i: Int, i1: Long): Long = {
    if (i <= 1) return i1
    return  factorialInt(i-1, i1*i)
  }

}

/*
solve(7466): [{(4),(6,6)}]  ----remove 7 solve for 466 : 5
                            -----remove 4 solve for 766 : 1
                            ---- remove 6 solve for 746:2 times *4 =8



solve(466):[{(4),(6,6)}] --- last el will be from (4)  6a,4/6b,4/6a,6b,4/6b,6a,4
                         ---last el from(6,6) then 6a,4,6b/6b,4,6a




*/


class ProblemETesting extends FunSuite {

 test("a") {
    ProblemETop("4 6 6")
 }

}
