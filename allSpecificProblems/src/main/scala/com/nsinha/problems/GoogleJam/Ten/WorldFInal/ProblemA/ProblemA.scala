package com.nsinha.problems.GoogleJam.Ten.WorldFInal.ProblemA

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 5/26/17.
  */
case class ProblemA(inStr : String) {
  var Alast = 0
  var Blast = 0
  var Clast = 0
  var cost = 0

  var stk = mutable.Stack[Char]()
  stk.push('d')

  precomputeLastOccurences
  computeCost
  println(cost)

  def print = {
    cost = cost + 1
  }

  def push(char : Char) = {
    stk.push(char)
    cost = cost + 1
  }

  def pop() = {
    stk.pop()
    cost = cost + 1
  }

  def precomputeLastOccurences = {
    var cnt = 0
    for (c ← inStr) {
      if (c == 'a') Alast = cnt
      if (c == 'b') Blast = cnt
      if (c == 'c') Clast = cnt
      cnt = cnt + 1
    }
  }

  def computeCost = {
    var cnt = 0
    for (c ← inStr) {
      if (stk.top == c) {
        print
      }
      else {
        var cond = true
        while (cond) {
          var popHappened = false
          if (stk.top == 'a' && cnt > Alast) {
            popHappened = true
            pop()
          }
          else if (stk.top == 'b' && cnt > Blast) {
            popHappened = true
            pop()
          }
          else if (stk.top == 'c' && cnt > Clast) {
            popHappened = true
            pop()
          }
          //did we reach our character
          if (stk.top == c) {
            print
            cond = false
          }
          else if (popHappened) {
            //nice pop happened and charcter still not macthing run the loop one more time
          }
          else {
            //no pop happened and charcter not matching do a push and print
            push(c)
            print
            cond = false
          }
        }
      }
      cnt = cnt + 1
    }
    cost = cost + stk.size - 1
  }
}

class ProblemATesting extends FunSuite {

  test ("a") {
    val str = "AAABAAB"
    ProblemA(str.toLowerCase())
  }

  test("b") {
    val str = "ABCCBA"
    ProblemA(str.toLowerCase())
  }

}
