package com.nsinha.problems.GoogleJam.Thirteen.WorldFinal.ProblemE

import org.scalatest.FunSuite

import scala.collection.mutable

/**
  * Created by nishchaysinha on 9/12/17.
  */

case class ProblemETop(inStr: String) {
  val input = inStr.split(" ") map {_.toInt}
  println(ProblemE(input).solveBrute())
}


case class ProblemE1Top(inStr: String) {
  val input = inStr.split(" ") map {_.toInt}
  val allMaximalLengths = ProblemE(input).findLongestSubSeqLengths

  val hashMap = mutable.HashMap[Int, Long]()

  allMaximalLengths foreach { maximalLen =>
    println(maximalLen)
  }
}

class CommonE(input:Array[Int]) {

  var problemIndices  = mutable.SortedMap[Int, Set[Int]]()

  var problemExist = false

  def generateProblemIndices(): mutable.Map[Int, Set[Int]] = {
    for (i <- Range(0, input.size)) {
      val curI = input(i)

      val ll = mutable.HashSet[Int]()

      for (j <- Range(i + 1, input.size)) {
        if (curI < input(j)) {
          problemExist = true
          ll += j
        }
      }
      problemIndices += i -> ll.toSet
    }
    problemIndices
  }
}


case class ProblemE(input: Array[Int]) {
  import ProblemE._

  val commonE = new CommonE(input)

  def findLongestSubSeqLengths: List[List[Int]] =  {
    import commonE._
    generateProblemIndices()
    val problemIndicesFiltered = problemIndices filter(_._2.nonEmpty)
    if (problemIndicesFiltered.isEmpty) {
      List(input.toList)
    } else {
      //pick the first problemIndices
      //remove both key and value set from input. call it in'
      //findLongestSub on in' call it S'. Append key and whole of value to S' and  getting A and B
      val kvhead = problemIndicesFiltered.head
      val setB = kvhead._2
      val setA = Set(kvhead._1)
      val setAB = kvhead._2.union(Set( kvhead._1))
      val newInputArray = getSubtractedArray(input, setAB).sorted
      val subproblemList = if (newInputArray.nonEmpty) ProblemE(newInputArray).findLongestSubSeqLengths else List[List[Int]]()
      if (subproblemList.nonEmpty) {
        val problem1List = subproblemList map { x =>
          val t = {setA.toList.sorted}  map (input(_))
          x ++ t
        }
        val problem2List = subproblemList map { x =>
          val t = {setB.toList.sorted}  map (input(_))
          x ++ t
        }
        problem1List ++ problem2List
      } else {
        List(setA.toList.sorted map (input(_)), setB.toList.sorted map (input(_)))
      }
    }
  }





  def solveBrute(): Long = {
    import commonE._
    if (cache.contains(input.foldLeft("") {(Z,el) => Z + el.toString})) {
      return cache(input.foldLeft("") {(Z,el) => Z + "-" +el.toString})
    }
    generateProblemIndices()

    if (!problemExist) return 1
   // if (!problemExist && lastIndicesSet.size == input.size) return factorial(input.size)

    val results = Range(0, input.size) map { i => solveThisIndexExcludedBrute(i) }

    val res = results.foldLeft(0L) {(Z,el) => Z + el}
    cache += (input.foldLeft("") {(Z,el) => Z + "-"+ el.toString}) -> res
    res
  }


  def solveThisIndexExcludedBrute(i: Int): Long = {
    import commonE._
    /* 1. Include this index
       2. Exclude any index that comes before this index for isolation (orthogonality) that will help us in not double counting
       3. For non problem indexes find all combinations of exc/include
       4. For problem indices have to remove every one of it.
      */
    val remainingArray = input.slice(0,i) ++ input.slice(i+1, input.size)//takes care of 1 and 2
    ProblemE(remainingArray).solveBrute()
  }
}


object ProblemE {

  val cache: mutable.HashMap[String, Long] = mutable.HashMap.empty

  def factorial(i: Int): Long = {
    factorialInt(i,1)
  }

  def factorialInt(i: Int, i1: Long): Long = {
    if (i <= 1) return i1
    return  factorialInt(i-1, i1*i)
  }

  def factorialUpto(i: Int, j: Int, res: Long = 1): Long = {
    if( i <= j) {
      return  res
    } else {
      factorialUpto(i-1, j, res * i)
    }
  }

  def nCombinem(i: Int, j: Int): Long = {
    factorialUpto(i,j)/factorial(j)
  }


  def getSubtractedArray(inp: Array[Int], setToDiff: Set[Int]): Array[Int] = {
    var inpMut = inp.clone()
    //remove set positions from inp
    for (indx <- setToDiff) {
      inpMut(indx) = -1
    }

    inpMut filter(x => x != -1)
  }

}



case class ProblemEETop(inStr: String) {
  val input = inStr.split(" ") map {_.toInt}

  ProblemEE(input)  solve()


}


case class ProblemEE(input: Array[Int]) {
  val hashMapForDp = mutable.HashMap[(Int, Int), Long]()
  val hashMapForDpOnSizeOnly = mutable.HashMap[Int, Long]()
  val hashMapForDpOnSizeOnlyEff = mutable.HashMap[Int, Long]()

  def createTheNonDecMap() = {
    for {
      size <- Range(1, input.size + 1)
      endIndex <- Range(0, input.size)
    } {
      hashMapForDp += ((endIndex, size)) -> 0
    }

    for {
      endIndex <- Range(0, input.size)
      size <- Range(1, input.size + 1)
    } {
      //add all elements that are prior to this and at same size

      if (size == 1) {
        hashMapForDp += ((endIndex, size)) -> 1
      } else { //size is not 1
        //in that case go through all size-1 that are also less than or equal to  current indexed number and add them together
        var sum = 0l
        //print("endIndex:" + endIndex)
        //println(" size:" + size)

        val lSize = size - 1
        for (lIndex <- Range(0, endIndex) if (input(lIndex) >= input(endIndex) )) {
          //print(s"considering index = $lIndex size = $lSize \t")
          sum = sum + hashMapForDp((lIndex, lSize))
        }

        //println()
        hashMapForDp += (endIndex, size) -> sum
      }
    }
  }


  def solve(): Long = {
    createTheNonDecMap()
    for {
      endIndex <- Range(0, input.size)
      size <- Range(1, input.size + 1)
    } {
      //println(endIndex, input(endIndex), size, hashMapForDp((endIndex,size)))
      if (hashMapForDpOnSizeOnly.contains(size)) {
        hashMapForDpOnSizeOnly += size -> (hashMapForDpOnSizeOnly(size) + hashMapForDp((endIndex,size)))
      } else {
        hashMapForDpOnSizeOnly += size -> hashMapForDp((endIndex,size))
      }
    }
    hashMapForDpOnSizeOnly += (input.size + 1) -> 0


    for (size <- Range(1, input.size +1)) {
      hashMapForDpOnSizeOnlyEff += size ->  {
        (hashMapForDpOnSizeOnly(size)* ProblemE.factorial(input.size - size) - hashMapForDpOnSizeOnly(size + 1)*(size + 1)* ProblemE.factorial(input.size - size - 1))
      }
    }

    var sum = 0L

    for (size <- Range(1, input.size +1)) {
      sum = sum + hashMapForDpOnSizeOnlyEff(size)
    }

    println(sum)
    sum



  }




}

/*
solve(7466): [{(4),(6,6)}]  ----remove 7 solve for 466 : 5
                            -----remove 4 solve for 766 : 1
                            ---- remove 6 solve for 746:2 times *4 =8



solve(466):[{(4),(6,6)}] --- last el will be from (4)  6a,4/6b,4/6a,6b,4/6b,6a,4
                         ---last el from(6,6) then 6a,4,6b/6b,4,6a




*/

/*
1. given a sequence for each node find the bad nodes and good nodes. o(n^2).eg 7466 : 7-> (),()(466) | 4 ->(6,6), (7)()|6->(),(
2.nFor a correct non dec sequence, one must have



 */


class ProblemETesting extends FunSuite {

 test("a") {
    ProblemEETop("4 3 1 2 1 1 2 2 3 4 2 2 5 4 3 1 3")
 }

}
