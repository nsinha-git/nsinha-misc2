package com.nsinha.problems.Ladder

import scala.collection.mutable

/** Created by nsinha on 3/13/17.
  */
case class Partition(n : Int, packSize : List[Int]) {

  private val memoize = mutable.Map[Int, List[List[Int]]]()

  def getAllPartition() : List[List[Int]] = {
    getAllPartitionsInt(n, List(List(0)))
  }

  private def getAllPartitionsInt(key : Int, carry : List[List[Int]]) : (List[List[Int]]) = {

    if (key == 0) {
      return carry
    }

    if (key < 0) {
      return List()
    }

    val allBranches = packSize map (x ⇒ key - x)

    val resOfThisRoot : List[List[Int]] = if (memoize.contains(key)) {
      memoize(key)
    }
    else {
      val allRes = allBranches zip packSize map { x ⇒
        val z = carry map { y ⇒ y ++ List(x._2) }
        println(s"z = $z")
        getAllPartitionsInt(x._1, z)
      }
      val curRes = allRes.foldLeft(List[List[Int]]()) { (Z, el) ⇒
        Z ++ el
      }

      memoize += key → curRes
      curRes
    }

    if (resOfThisRoot.isEmpty) return Nil else {
      resOfThisRoot
    }
  }

}

object Testing {

  def main(args : Array[String]) : Unit = {

    val t = Partition(3, List(1, 2, 3))
    val s = t.getAllPartition()
    println(s)

  }
}
