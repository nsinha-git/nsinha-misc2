package com.nsinha.problems.GoogleJam.Eleven.WorldFinal.ProblemA

import org.scalatest.FunSuite

import scala.collection.immutable.TreeMap
import scala.collection.mutable

/** Created by nsinha on 5/24/17.
  */
case class DpKey(set : TreeMap[Char, Int], prefixCharOpt : Option[Char], runsRequired : Int) {
  /*override def toString: String = {
    val freqs = {set map {_._2} toList}.sorted
    val hashString = set.foldLeft(""){(Z,el) => Z + el }
    hashString + prefixCharOpt.toString + runsRequired.toString
  }*/

  override def toString : String = {
    val freqs = { set map { _._2 } toList }.sorted
    //val hashString = set.foldLeft(""){(Z,el) => Z + el }
    val hashString = freqs.foldLeft(""){ (Z, el) ⇒ Z+"_"+el }
    val prefixTag : String = {
      prefixCharOpt match {
        case None ⇒ "None"
        case _    ⇒ "Some"
      }
    }
    //hashString + prefixCharOpt.toString + runsRequired.toString
    hashString + prefixTag + runsRequired.toString
  }
}

object DP {

  val table = mutable.HashMap[String, Long]()
  var tablehits = 0

  val tableZeroedHighestSeen = mutable.HashMap[(Int, Boolean), List[Int]]()

}

case class DP(dpKey : DpKey) {
  import DP._

  def totalEntries(t : Map[Char, Int]) : Int = {
    var cnt = 0
    t foreach (x ⇒ cnt = cnt + x._2)
    cnt
  }

  def heuristicalMaxBoundsRuns(set : TreeMap[Char, Int]) : Int = {
    var sum = 0
    var lastFreq = 0
    set foreach {
      case (char, freq) ⇒
        sum = sum + 2 * lastFreq
        val temp = freq - lastFreq
        lastFreq = temp
    }
    sum
  }

  def solve : Long = {
    if (table.contains(dpKey.toString)) {
      tablehits = tablehits + 1
      table(dpKey.toString)
    }
    else if (dpKey.set.isEmpty) {
      if (dpKey.runsRequired == 0) return 1 else 0
    }
    else if (dpKey.runsRequired == 0) {
      dpKey.prefixCharOpt match {
        case None    ⇒ 0
        case Some(x) ⇒ if (dpKey.set.size == 1 && dpKey.set.keys.head == x) 1 else 0
      }
    }
    else {
      if (dpKey.set.size > (dpKey.runsRequired + 1)) {
        0
      }
      else if (totalEntries(dpKey.set) <= dpKey.runsRequired) {
        0
      }
      else if (heuristicalMaxBoundsRuns(dpKey.set) < dpKey.runsRequired) {
        0
      }
      else {
        var sum = 0L
        dpKey.prefixCharOpt match {
          case None ⇒
            dpKey.set.keys.foreach { curChar ⇒ sum = sum + subproblemSum(curChar, false) }
          case Some(prefixChar) ⇒
            dpKey.set.keys.foreach { curChar ⇒
              if (curChar == prefixChar) {
                sum = sum + subproblemSum(curChar, true)
              }
              else {
                sum = sum + subproblemSum(curChar, false)
              }
            }
        }
        table += dpKey.toString → sum
        sum
      }
    }
  }

  def subproblemSum(curChar : Char, matchesPrefix : Boolean) : Long = {
    val isMoreCurChars = if ((dpKey.set(curChar) - 1) > 0) true else false
    val newSet = if (isMoreCurChars) {
      dpKey.set + (curChar → (dpKey.set(curChar) - 1))
    }
    else {
      dpKey.set - curChar
    }

    val subDpKey = if (isMoreCurChars) {
      if (matchesPrefix) {
        new DpKey(newSet, Option(curChar), dpKey.runsRequired)
      }
      else {
        new DpKey(newSet, Option(curChar), dpKey.runsRequired - 1)
      }
    }
    else {
      if (matchesPrefix) {
        new DpKey(newSet, None, dpKey.runsRequired)
      }
      else {
        new DpKey(newSet, None, dpKey.runsRequired - 1)
      }
    }
    DP(subDpKey).solve
  }

}
class ProblemA(s : String) {
  DP.table.empty
  DP.tablehits = 0

  val mapOfAlphas : TreeMap[Char, Int] = createMap

  val targetRun = findRun

  val dpKey = new DpKey(mapOfAlphas, None, targetRun)

  println(DP(dpKey).solve)
  println(DP.table.size)
  println(DP.tablehits)
  println(DP.table)

  def findRun() : Int = {
    var run = 0
    var prevChar : Char = 0
    s foreach { curChar ⇒
      if (curChar != prevChar) {
        prevChar = curChar
        run = run + 1
      }
    }
    run
  }

  def createMap = {
    val treeMap = mutable.TreeMap[Char, Int]()
    for (c ← s) {
      if (treeMap.contains(c)) { treeMap += c → (treeMap(c) + 1) } else { treeMap += c → (1) }
    }
    treeMap.foldLeft(TreeMap[Char, Int]()) { (Z, el) ⇒
      Z + el
    }
  }

}

class ProblemATesting extends FunSuite {
  test ("a") {
    //new ProblemA("aab")

    //new ProblemA("aaabbc")
    //new ProblemA("aaabbbc")
    //new ProblemA("aabbcc")
    //new ProblemA("abcd")
    new ProblemA("abcdabcda")

    //new ProblemA("aabcd")

    //new ProblemA("bookkeeper")

  }
  test("b") {
    new ProblemA("ccccuuuugggggghhhhhhhhhhhbbbbbbbbbbbbbbblllllllllllllllccccccqqqqqqqqqqqqzzjjjjjjjjjjjjjjjxxxxxxxxxxxuuuuhhhhhhhhhllllyyyyyyyyygggggggcccccccccccccxxxxmmmmmmgggggggggggggzzlllccxxxxjjjjjjjjjuuuuuuqqqqqqqqqqyxllluuuuuuuuuuuhhhhhhhhhhjjjjjjjjjjjjjzzzzzzzzzzzzxcccccccccggggggggggggggghhbbbbbbbbccccccccqqqqqqqqqqqqqttttttttttttttaaaaatttttttrrrrrrzzzzzzzzzzmmmmmmmmmmmmcccccccwwwwhhhhhhhhrrrrrrrrrrxxxjjjjjjjjjjbwwwwwwwwbbqqaaaaaaaaaaauuuuuuummmmmmmmmmmmmmmzzzcccccccmmmmjjjjuuuuuuuuuuuuuutttggggggxxxxxqqqqqqqqqqqqqrrrrrrrrrrrrrrryyyyyyzzzqqqqqqqqbbbbbbbbbbbbbbbyyyyyyyyyyyyyyyyzzzzzzzzzzzzzyyyuuuuuuuuuuuxxxxllllttwwwwwdduudddyxxxmmmmmmmmmmmccccccccccbbbhhhhhhhhhhhhhhhgglllcggbbbbbbbbbbbbaayyttttttttttt")
  }

}
