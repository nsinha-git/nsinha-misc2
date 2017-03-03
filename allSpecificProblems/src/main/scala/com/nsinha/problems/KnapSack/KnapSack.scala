package com.nsinha.problems.KnapSack

import scala.collection.mutable
import org.scalatest._
/** Created by nsinha on 2/28/17.
  */
case class Weight(value : Int)
case class Value(value : Int)
class KnapSack(goods : List[(String, Weight, Value)], maxCap : Weight) {
  var dpTable = mutable.Map[(Weight, List[String]), Int]()
  val namesOfAllGoods = goods map (_._1)
  val goodsToValue = goods map { x ⇒ (x._1, x._3) } toMap
  val goodsToWeight = goods map { x ⇒ (x._1, x._2) } toMap

  def solve : (Int, List[String]) = {
    solveInt(maxCap, namesOfAllGoods)
  }

  def solveInt(weightRemaining : Weight, names : List[String]) : (Int, List[String]) = {
    val res = names.foldLeft((0, List[String]())){
      (z, good) ⇒
        val valueOfGood = goodsToValue(good)
        val weightOfGood = goodsToWeight(good)
        val remainingWeight = weightRemaining.value - weightOfGood.value
        if (remainingWeight > 0) {
          val bestPathForThisSel = solveInt(Weight(remainingWeight), names filter (x ⇒ x != good))
          if (z._1 < (bestPathForThisSel._1 + valueOfGood.value)) {
            (bestPathForThisSel._1 + valueOfGood.value, List(good) ++ bestPathForThisSel._2)
          }
          else {
            z
          }
        }
        else {
          z
        }
    }
    dpTable += ((weightRemaining, names) → res._1)
    res
  }
}

class Testing extends FunSuite {

  test("123") {
    val p = new KnapSack(
      List(("g1", Weight(10), Value(100)), ("g2", Weight(4), Value(70)), ("g3", Weight(6), Value(50)), ("g4", Weight(12), Value(10))),
      Weight(12)
    )
    val res = p.solve
    println(res)
  }
}

