package com.nsinha.problems.GoogleJam.Fourteen.WorldFinal.ProblemE

import org.scalatest.FunSuite

/**
  * Created by nishchaysinha on 10/23/17.
  */
case class ProblemE(inStr: String) {

  def decompose(str: String): Array[Int] = {
    str.split(" ") map (x => x.toInt)
  }

  def solve: Int = {
    val inp = decompose(inStr)
    val N = inp(0)
    val A = inp(1)
    val B = inp(2)
    val k = searchMinimum(N,A,B)
    //k is kind of approximate value. we need to figure out the true minimum based on this
    //a heuristic can be used that says always pick the ceiling after dividing an odd number as the worst case sceneario is what
    // we may have to encounter
    var curN = N
    var cost = 0
    for (i <- Range(0, k)) {
      if (curN%2 == 0) {
        curN = curN/2
        cost = cost + B
      } else {
        curN = (curN-1)/2
        cost = cost + B
      }
    }
    cost = cost + (curN -1) * A
    cost
  }

  def log2 (n: Int): Int = {
    for (k <- Range(0, 20)) {
      import math.pow
      if (pow(2, k) >= n) return k
    }
    return 20
  }

  def searchMinimum(N: Int, A: Int, B: Int): Int = {
    var minIndx = 0
    var minValue: Double = N*B
    for (k <- Range(0,log2(N))) {
      val curVal = (N/math.pow(2,k) -1) * A + k*B
      if (curVal < minValue){
        minValue = curVal
        minIndx = k
      }
    }
    minIndx
  }

}



class ProblemETesting extends FunSuite {

  test ("1") {
    println(ProblemE("4 5 7").solve)
    println(ProblemE("8 1 1").solve)
    println(ProblemE("1 23 32").solve)
  }

  test("2"){

    1 2 3 4 5 3 6

  }



}
