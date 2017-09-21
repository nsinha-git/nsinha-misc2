package com.nsinha.problems.GoogleJam.Twelve.WorldFinal.ProblemE

import org.scalatest.FunSuite

import scala.collection.mutable

/**
  * Created by nishchaysinha on 8/28/17.
  */
class ProblemE(in: List[(Int, Int, Int)]) {

  val visitedStates = mutable.HashSet[String]()

  val mapOfLREdges: Map[Int, (Int, Int)] = {
    val mp = mutable.HashMap[Int, (Int, Int)] ()

    for (el <- in) {
      mp += el._1 -> (el._2, el._3)
    }
    mp toMap
  }

  val n = in.size + 1
  val highestNumItr = n*math.pow(2, n).toInt


  var curState = {
    val x = mutable.TreeMap[Int, Boolean]()
    for ( i <- Range(0, n)) {
      x(i) = false
    }
    visitedStates += (createStringOfMutableMap(x))
    x
  }
  var curLoc = 1

  def createStringOfMutableMap(state: mutable.TreeMap[Int, Boolean]): String = {
    val s = new mutable.StringBuilder()
    for (i<-state) {
      s.append(i._2)
    }
    s.result()
  }



  def isFinal = {
    curState(n-1)
  }


  def solve: Long = {
    var res = 0L

    for (i <- Range(0,highestNumItr)) {
      if (curState(curLoc - 1) == true) {
        val newLoc = mapOfLREdges(curLoc)._2
        curState(curLoc-1) = false
        curLoc = newLoc
        if(visitedStates.contains(createStringOfMutableMap(curState))) {
          return -1
        } else {
          visitedStates +=createStringOfMutableMap(curState)
        }

      } else {
        val newLoc = mapOfLREdges(curLoc)._1
        curState(curLoc - 1) = true
        curLoc = newLoc
        if(visitedStates.contains(createStringOfMutableMap(curState))) {
          return -1
        } else {
          visitedStates +=createStringOfMutableMap(curState)
        }

      }
      res = res + 1
      if (isFinal) return res
    }
    return -1
  }

}




class ProblemETesting extends FunSuite {

  test ("1") {
    //4 2 1 3 1 2 4
    val p = new ProblemE(List((1,2,1),(2,3,1), (3,2,4)))

    println(p.solve)


  }

}
