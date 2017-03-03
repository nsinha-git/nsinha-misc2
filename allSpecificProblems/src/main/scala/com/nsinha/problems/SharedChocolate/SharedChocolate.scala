package com.nsinha.problems.SharedChocolate
import scala.collection.mutable
import org.scalatest.FunSuite

/** Created by nsinha on 3/2/17.
  */

case class Coord(x : Int, y : Int) {
  def invert = Coord(y, x)
}

case class SharedChocolate(size : Coord, candidates : List[Int], dpTable : mutable.Map[(Coord, String), Option[Int]] = mutable.Map(),
                           dpVer2 : mutable.Map[(Int, String), (Int, Option[Int])] = mutable.Map()) {
  val totalBlocksInThisBox = size.x * size.y

  val totalCandidatesBlockRequired = candidates.foldLeft(0) { (Z, el) ⇒
    Z + el
  }

  private val subSet : List[(List[Int], List[Int])] = {
    val ll : Array[Int] = candidates.toArray
    if (ll.size == 0) {
      Nil
    }
    else {
      val res = Range(0, math.pow(2, candidates.size).toInt).toList map {
        x ⇒
          var y = x
          var cond = true
          var bitCnt = 0
          var yesList = mutable.MutableList[Int]()
          var noList = mutable.MutableList[Int]()

          while (bitCnt < candidates.size) {
            var curBit = y & 0x1
            y = y >>> 1
            if (curBit == 1) {
              yesList += ll(bitCnt)
            }
            else {
              noList += ll(bitCnt)
            }
            bitCnt = bitCnt + 1
          }
          (yesList.toList, noList.toList)
      }
      res
    }
  }

  //returns the number of optimal cuts
  def solve : Option[Int] = {
    //we can cut this node in length or width
    val cutsLength : Seq[Int] = Range(1, size.x)
    val cutsWidth : Seq[Int] = Range(1, size.y)

    //base cases
    if (totalBlocksInThisBox < totalCandidatesBlockRequired) return None

    if (candidates.size == 0) return Option(0)

    val stringRepForCandidates = candidates.sorted.mkString("-")

    if (dpTable.contains((size, stringRepForCandidates))) {
      return dpTable((size, stringRepForCandidates))
    }

    if (totalBlocksInThisBox == totalCandidatesBlockRequired && candidates.size == 1) {
      dpTable += (size, candidates.mkString("-")) → Option(0)
      dpTable += (size.invert, candidates.mkString("-")) → Option(0)
      return Option(0)
    }

    var min1 : Option[Int] = None
    var min2 : Option[Int] = None

    if (cutsLength.size >= 1) {
      val valuesForLengthCuts = cutsLength map {
        cut ⇒
          val box1 = Coord(size.x - cut, size.y)
          val box2 = Coord(cut, size.y)
          val allTuplesOfCandidates : List[(List[Int], List[Int])] = subSet
          allTuplesOfCandidates.foldLeft(None : Option[Int]) { (Z, el) ⇒
            val el1StringRep = el._1.sorted.mkString("-")
            val el2StringRep = el._2.sorted.mkString("-")

            val aOpt = { SharedChocolate(box1, el._1, dpTable) }.solve

            aOpt match {
              case None ⇒
                dpTable += ((box1, el1StringRep) → None)
                dpTable += ((box1.invert, el1StringRep) → None)
              case Some(a) ⇒
                dpTable += ((box1, el1StringRep) → aOpt)
                dpTable += ((box1.invert, el1StringRep) → aOpt)
            }
            val bOpt = { SharedChocolate(box2, el._2, dpTable) }.solve

            bOpt match {
              case None ⇒
                dpTable += ((box2, el2StringRep) → None)
                dpTable += ((box2.invert, el2StringRep) → None)
              case Some(a) ⇒
                dpTable += ((box2, el2StringRep) → bOpt)
                dpTable += ((box2.invert, el2StringRep) → bOpt)
            }

            val sumOfCutsOpt = aOpt flatMap { a ⇒ bOpt map { b ⇒ (a + b + 1) } }
            sumOfCutsOpt match {
              case Some(sumOfCuts) ⇒ Z match {
                case None ⇒ sumOfCutsOpt
                case Some(z) ⇒
                  if (z < sumOfCuts) Z else Option(sumOfCuts)
              }
              case None ⇒ Z
            }
          }
      }
      min1 = valuesForLengthCuts.foldLeft(None : Option[Int]) { (Z, el) ⇒
        (el, Z) match {
          case (None, _)            ⇒ Z
          case (Some(ell), None)    ⇒ el
          case (Some(ell), Some(z)) ⇒ if (z > ell) Option(ell) else Z
        }
      }
    }

    if (cutsWidth.size >= 1) {
      val valuesForWidthCuts = cutsWidth map {
        cut ⇒
          val box1 = Coord(size.x, cut)
          val box2 = Coord(size.x, size.y - cut)
          val allTuplesOfCandidates : List[(List[Int], List[Int])] = subSet
          allTuplesOfCandidates.foldLeft(None : Option[Int]) { (Z, el) ⇒
            val el1StringRep = el._1.sorted.mkString("-")
            val el2StringRep = el._2.sorted.mkString("-")

            val aOpt = { SharedChocolate(box1, el._1, dpTable) }.solve

            aOpt match {
              case None ⇒
                dpTable += ((box1, el1StringRep) → None)
                dpTable += ((box1.invert, el1StringRep) → None)
              case Some(a) ⇒
                dpTable += ((box1, el1StringRep) → aOpt)
                dpTable += ((box1.invert, el1StringRep) → aOpt)
            }
            val bOpt = { SharedChocolate(box2, el._2, dpTable) }.solve

            bOpt match {
              case None ⇒
                dpTable += ((box2, el2StringRep) → None)
                dpTable += ((box2.invert, el2StringRep) → None)
              case Some(a) ⇒
                dpTable += ((box2, el2StringRep) → bOpt)
                dpTable += ((box2.invert, el2StringRep) → bOpt)
            }

            val sumOfCutsOpt = aOpt flatMap { a ⇒ bOpt map { b ⇒ (a + b + 1) } }
            sumOfCutsOpt match {
              case Some(sumOfCuts) ⇒ Z match {
                case None ⇒ sumOfCutsOpt
                case Some(z) ⇒
                  if (z < sumOfCuts) Z else Option(sumOfCuts)
              }
              case None ⇒ Z
            }
          }
      }
      min2 = valuesForWidthCuts.foldLeft(None : Option[Int]) { (Z, el) ⇒
        (el, Z) match {
          case (None, _)            ⇒ Z
          case (Some(ell), None)    ⇒ el
          case (Some(ell), Some(z)) ⇒ if (z > ell) Option(ell) else Z
        }
      }
    }

    val res = (min1, min2) match {
      case (None, _)          ⇒ min2
      case (_, None)          ⇒ min1
      case (Some(x), Some(y)) ⇒ if (x > y) min2 else min1
    }

    dpTable += ((size, stringRepForCandidates) → res)
    dpTable += ((size.invert, stringRepForCandidates) → res)

    res
  }

  //returns the number of optimal cuts
  def solveVer2 : Option[Int] = {
    //we can cut this node in length or width
    val cutsLength : Seq[Int] = Range(1, size.x)
    val cutsWidth : Seq[Int] = Range(1, size.y)

    //base cases
    if (totalBlocksInThisBox < totalCandidatesBlockRequired) return None

    if (candidates.size == 0) return Option(0)

    val stringRepForCandidates = candidates.sorted.mkString("-")

    if (dpVer2.contains((size.x, stringRepForCandidates))) {
      val tryst = dpVer2((size.x, stringRepForCandidates))
      tryst._2 match {
        case None ⇒ if (size.y <= tryst._1) return None
        case Some(rr) ⇒ if (size.y < tryst._1) {
          return None
        }
        else if (size.y == tryst._1) {
          return tryst._2
        }
        else {
          return tryst._2 map (_ + 1)
        }
      }
    }

    if (totalBlocksInThisBox == totalCandidatesBlockRequired && candidates.size == 1) {
      dpVer2 += (size.x, candidates.mkString("-")) → (size.y, Option(0))
      return Option(0)
    }

    var min1 : Option[Int] = None
    var min2 : Option[Int] = None

    if (cutsLength.size >= 1) {
      val valuesForLengthCuts = cutsLength map {
        cut ⇒
          val box1 = Coord(size.x - cut, size.y)
          val box2 = Coord(cut, size.y)
          val allTuplesOfCandidates : List[(List[Int], List[Int])] = subSet
          allTuplesOfCandidates.foldLeft(None : Option[Int]) { (Z, el) ⇒
            val el1StringRep = el._1.sorted.mkString("-")
            val el2StringRep = el._2.sorted.mkString("-")

            val aOpt = { SharedChocolate(box1, el._1, dpTable, dpVer2) }.solveVer2

            aOpt match {
              case None ⇒
                if (dpVer2.contains((box1.x, el1StringRep))) {
                  val tryst = dpVer2((box1.x, el1StringRep))
                  tryst._2 match {
                    case None ⇒ if (box1.y > tryst._1) {
                      dpVer2.remove((box1.x, el1StringRep))
                      dpVer2 += ((box1.x, el1StringRep) → (box1.y, None))
                    }
                    case Some(rr) ⇒ None
                  }
                }
              case Some(a) ⇒ /*we can assume dpVer2 must contain this entry*/
              // dont need any update dpTable += ((box1, el1StringRep) → aOpt)
            }

            val bOpt = { SharedChocolate(box2, el._2, dpTable, dpVer2) }.solveVer2

            bOpt match {
              case None ⇒
                if (dpVer2.contains((box2.x, el2StringRep))) {
                  val tryst = dpVer2((box2.x, el2StringRep))
                  tryst._2 match {
                    case None ⇒ if (box2.y > tryst._1) {
                      dpVer2.remove((box2.x, el2StringRep))
                      dpVer2 += ((box2.x, el2StringRep) → (box1.y, None))
                    }
                    case Some(rr) ⇒ None
                  }
                }

              case Some(a) ⇒
            }

            val sumOfCutsOpt = aOpt flatMap { a ⇒ bOpt map { b ⇒ (a + b + 1) } }
            sumOfCutsOpt match {
              case Some(sumOfCuts) ⇒ Z match {
                case None ⇒ sumOfCutsOpt
                case Some(z) ⇒
                  if (z < sumOfCuts) Z else Option(sumOfCuts)
              }
              case None ⇒ Z
            }
          }
      }
      min1 = valuesForLengthCuts.foldLeft(None : Option[Int]) { (Z, el) ⇒
        (el, Z) match {
          case (None, _)            ⇒ Z
          case (Some(ell), None)    ⇒ el
          case (Some(ell), Some(z)) ⇒ if (z > ell) Option(ell) else Z
        }
      }
    }

    if (cutsWidth.size >= 1) {
      val valuesForWidthCuts = cutsWidth map {
        cut ⇒
          val box1 = Coord(size.x, cut)
          val box2 = Coord(size.x, size.y - cut)
          val allTuplesOfCandidates : List[(List[Int], List[Int])] = subSet
          allTuplesOfCandidates.foldLeft(None : Option[Int]) { (Z, el) ⇒
            val el1StringRep = el._1.sorted.mkString("-")
            val el2StringRep = el._2.sorted.mkString("-")

            val aOpt = { SharedChocolate(box1, el._1, dpTable, dpVer2) }.solveVer2

            aOpt match {
              case None ⇒
                if (dpVer2.contains((box1.x, el1StringRep))) {
                  val tryst = dpVer2((box1.x, el1StringRep))
                  tryst._2 match {
                    case None ⇒ if (box1.y > tryst._1) {
                      dpVer2.remove((box1.x, el1StringRep))
                      dpVer2 += ((box1.x, el1StringRep) → (box1.y, None))
                    }
                    case Some(rr) ⇒ None
                  }
                }
              case Some(a) ⇒ /*we can assume dpVer2 must contain this entry*/
              // dont need any update dpTable += ((box1, el1StringRep) → aOpt)
            }

            val bOpt = { SharedChocolate(box2, el._2, dpTable, dpVer2) }.solveVer2

            bOpt match {
              case None ⇒
                if (dpVer2.contains((box2.x, el2StringRep))) {
                  val tryst = dpVer2((box2.x, el2StringRep))
                  tryst._2 match {
                    case None ⇒ if (box2.y > tryst._1) {
                      dpVer2.remove((box2.x, el2StringRep))
                      dpVer2 += ((box2.x, el2StringRep) → (box1.y, None))
                    }
                    case Some(rr) ⇒ None
                  }
                }

              case Some(a) ⇒
            }

            val sumOfCutsOpt = aOpt flatMap { a ⇒ bOpt map { b ⇒ (a + b + 1) } }
            sumOfCutsOpt match {
              case Some(sumOfCuts) ⇒ Z match {
                case None ⇒ sumOfCutsOpt
                case Some(z) ⇒
                  if (z < sumOfCuts) Z else Option(sumOfCuts)
              }
              case None ⇒ Z
            }
          }
      }

      min2 = valuesForWidthCuts.foldLeft(None : Option[Int]) { (Z, el) ⇒
        (el, Z) match {
          case (None, _)            ⇒ Z
          case (Some(ell), None)    ⇒ el
          case (Some(ell), Some(z)) ⇒ if (z > ell) Option(ell) else Z
        }
      }
    }

    val res = (min1, min2) match {
      case (None, _)          ⇒ min2
      case (_, None)          ⇒ min1
      case (Some(x), Some(y)) ⇒ if (x > y) min2 else min1
    }

    res match {
      case None ⇒
        if (dpVer2.contains((size.x, stringRepForCandidates))) {
          val tryst = dpVer2((size.x, stringRepForCandidates))
          tryst._2 match {
            case None ⇒ if (size.y > tryst._1) {
              dpVer2.remove((size.x, stringRepForCandidates))
              dpVer2 += ((size.x, stringRepForCandidates) → (size.y, None))
            }
            case Some(rr) ⇒
          }
        }
        else {
          dpVer2 += ((size.x, stringRepForCandidates) → (size.y, None))
        }
      case Some(resres) ⇒ /*you coming here for first time pretty much
      means this minimum Wmin or min exists. We only change if key does not exist
      */
        if (dpVer2.contains((size.x, stringRepForCandidates))) {
          dpVer2((size.x, stringRepForCandidates))._2 match {
            case None ⇒
              dpVer2 += ((size.x, stringRepForCandidates) → (size.y, res))
            case _ ⇒ //Dont do anything
          }
        }

    }

    res
  }

}

class Test extends FunSuite {

  test ("a") {
    val problem = new SharedChocolate(Coord(3, 3), List(4, 1, 1))
    println(problem.solve)
  }

  test ("impossible") {
    val problem = new SharedChocolate(Coord(3, 3), List(5, 1, 1))
    println(problem.solve)
    println(problem.solve)
  }

  test ("aver2") {
    val problem = new SharedChocolate(Coord(3, 3), List(4, 1, 1))
    println(problem.solveVer2)
    println(problem.solveVer2)
  }

  test ("impossible ver2") {
    val problem = new SharedChocolate(Coord(3, 3), List(5, 1, 1))
    println(problem.solveVer2)
    println(problem.solveVer2)
  }
}
