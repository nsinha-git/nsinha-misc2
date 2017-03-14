package com.nsinha.problems.Ladder

import scala.collection.mutable

/** Created by nsinha on 3/13/17.
  */

/* space delimited input
run two maps(straight and inverted) greedily by matching the pattern chars to input ctokens.
if map breaks down either way we have a failure in match. Report eagerly for optimal but non-asymptotic time.

Analysis Time: O(min(len(pattern), len(input))
Space O(2*min(len(pattern), len(input)

Linear in time , linear in memory

Points to remember:
String is indexed in scala. So random acesss in o(1)
 */
case class Tokens(pattern : String, input : String) {
  val inputTokens = input.split(" ").toList
  var nextPatternTry = 0
  var nextTokenTry = 0

  val patToToken = mutable.Map[Char, String]()
  val tokenToPat = mutable.Map[String, Char]()

  def getNextPatternCharTry : Option[Char] = {
    if (nextPatternTry < pattern.size) {
      nextPatternTry = nextPatternTry + 1
      Option(pattern(nextPatternTry - 1))
    }
    else {
      None
    }
  }

  def getNextTokenTry : Option[String] = {
    if (nextTokenTry < inputTokens.size) {
      nextTokenTry = nextTokenTry + 1
      Option(inputTokens(nextTokenTry - 1))
    }
    else {
      None
    }
  }

  def isMatch : Boolean = {
    var cond = true
    var res = true
    while (cond) {
      val nextPat = getNextPatternCharTry
      val nextToken = getNextTokenTry
      (nextPat, nextToken) match {
        case (None, None) ⇒
          cond = false
          return res
        case (Some(pat), None) ⇒
          cond = false
          res = false
          return res
        case (None, Some(token)) ⇒
          cond = false
          res = false
          return res

        case (Some(pat), Some(token)) ⇒
          if (patToToken.contains(pat)) {
            if (patToToken(pat) != token) {
              cond = false
              res = false
              return res
            }
          }

          if (tokenToPat.contains(token)) {
            if (tokenToPat(token) != pat) {
              cond = false
              res = false
              return res
            }
          }
          // pat or token are not in their maps.
          //this proves both pat and token have never been seen before
          //we should just enter them in their maps and be gone for time being
          patToToken += pat → token
          tokenToPat += token → pat
      }

    }

    res

  }
}

object ABC {

  def main(args : Array[String]) : Unit = {
    val t1 = Tokens("aba", "red blue red")
    println("t1:"+t1.isMatch)
    val t2 = Tokens("aba", "red red red")
    println("t2:"+t2.isMatch)

  }
}

