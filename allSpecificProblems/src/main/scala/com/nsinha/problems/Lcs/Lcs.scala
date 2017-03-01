package com.nsinha.problems.Lcs

import scala.collection.mutable
import org.scalatest.{FunSuite, MustMatchers}

/** Created by nsinha on 2/27/17.
  */
case class Len(x : Int)
case class RightEnd(x : Int)
case class LeftEnd(x : Int)

object Utils {
  implicit def toInt(l : Len) : Int = l.x
  implicit def toInt(l : LeftEnd) : Int = l.x
  implicit def toInt(l : RightEnd) : Int = l.x
}

/*
input: array of n -ordered objects
output: a array of objects which is subset that is longest subsequence
*/
class Lcs(input : List[Int]) {
  import Utils._
  val mapOfSurvivors = mutable.Map[Len, (List[Int], Int)]()

  def stepk(k : Int) = {
    val curVal = input(k)
    //highest len survivor benefitting from curVal
    val candidates = mapOfSurvivors.filter(x ⇒ (x._2._2 <= curVal))
    val newHigestLen : (Len, (List[Int], Int)) = if (candidates.nonEmpty) {
      val higestLen = candidates.maxBy(_._1.x)
      (Len(higestLen._1 + 1), (higestLen._2._1 :+ curVal, curVal))
    }
    else {
      (Len(1), (List(curVal), curVal))
    }
    mapOfSurvivors.get(newHigestLen._1) match {
      case None    ⇒ mapOfSurvivors += (newHigestLen)
      case Some(x) ⇒ if (x._2 > newHigestLen._2._2) mapOfSurvivors += (newHigestLen)
    }
  }

  def run() = {
    Range(0, input.length) foreach (k ⇒ stepk(k))
    val max = mapOfSurvivors.maxBy(_._1.x)
    println(max)
  }
}

class Testing extends FunSuite {
  test("10 8 9  5 6 7 1 2 3 4") {
    val in = List(10, 8, 9, 5, 6, 7, 1, 2, 3, 4)
    val lcs = new Lcs(in)
    lcs.run()
  }

  test("-7 10  9  2 3 8 8 1") {
    val in = List(-7, 10, 9, 2, 3, 8, 8, 1)
    val lcs = new Lcs(in)
    lcs.run()
  }
}
