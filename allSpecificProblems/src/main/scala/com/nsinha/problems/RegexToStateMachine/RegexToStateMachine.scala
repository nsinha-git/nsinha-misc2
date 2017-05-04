package com.nsinha.problems.RegexToStateMachine

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 4/14/17.
  */

class RegexNode(isFinal : Boolean, name : String) {
  var edges : Map[String, RegexNode] = _

}

class RegexToStateMachine(str : String) {

}

class KnuthMorrisPrat {

  def runOn(target : String) : mutable.Map[Int, Int] = {
    val mp = mutable.TreeMap[Int, Int]()

    for (mismatchAt ← Range(0, target.length)) {
      val shiftAtMisMatch = getShifts(mismatchAt, target)
      mp += mismatchAt → shiftAtMisMatch
    }

    mp
  }

  def getShifts(at : Int, target : String) : Int = {
    var shifts = at
    for (i ← Range(1, at)) {
      if (i < shifts) {
        val s1 = target.substring(0, at - i + 1)
        val s2 = target.substring(i, at + 1)
        if (s1 == s2) shifts = i
      }
    }
    math.max(shifts, 1)
  }

}

class RegexToStateMachineTesting extends FunSuite {
  test ("a") {
    val c = new KnuthMorrisPrat
    println(c.runOn("ababaca"))
  }

}
