package com.nsinha.problems.RegexToStateMachine

import org.scalatest.FunSuite

/** Created by nsinha on 4/14/17.
  */
class RegexToStateMachine(regex : String) {

  def runOn(target : String) : Array[Int] = {
    ???
  }

}

class RegexToStateMachineTesting extends FunSuite {
  test ("a") {
    val c = new RegexToStateMachine("abc")
    // c.runOn("abcdefabc")
  }

}
