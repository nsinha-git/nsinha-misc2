package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemC

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 4/15/17.
  */
case class AlphabhetDpKey(alphabhets : Int, totalRows : Int, totalCols : Int, row : Int, col : Int) {
  override def hashCode() = alphabhets.hashCode() + totalRows.hashCode() + totalCols.hashCode() + row.hashCode() + col.hashCode()

  override def equals(obj : scala.Any) : Boolean = if (!obj.isInstanceOf[AlphabhetDpKey]) {
    false
  }
  else {
    val that = obj.asInstanceOf[AlphabhetDpKey]
    that.alphabhets == alphabhets && that.col == col && that.row == row && that.totalRows == totalRows && that.totalCols == totalCols
  }
}

case class ProblemC(totalRows : Int, totalCols : Int, alphabets : Int, dpS : mutable.HashMap[AlphabhetDpKey, Long] = mutable.HashMap[AlphabhetDpKey, Long](), dpTS : mutable.HashMap[AlphabhetDpKey, Long] = mutable.HashMap[AlphabhetDpKey, Long]()) {

  def solve : Long = {
    if (totalRows == 0 || totalCols == 0) return 1
    solveInt
    solveDirect
  }

  def solveDirect : Long = {
    if (totalRows == 0 || totalCols == 0) return 1

    val res = Range(1, alphabets + 1).foldLeft(0L) { (Z, el) ⇒ Z + dpTS(AlphabhetDpKey(el, totalRows, totalCols, totalRows - 1, totalCols - 1)) }

    println(totalRows+":"+totalCols+":"+alphabets+"="+res)
    res
  }

  def solveInt = {
    if (alphabets > 1) {
      if (!dpTS.contains(AlphabhetDpKey(alphabets - 1, totalRows, totalCols, totalRows - 1, totalCols - 1))) ProblemC(totalRows, totalCols, alphabets - 1, dpS, dpTS) solve
    }
    // at this time we have lower aplphbet completely solved or cardinality is 1
    if (alphabets == 1) {
      Range(0, totalRows) foreach { i ⇒
        Range(0, totalCols) foreach { j ⇒
          dpS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j)) = 1
          dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j)) = 1
        }
      }
    }
    else {
      if (!dpTS.contains(AlphabhetDpKey(alphabets, totalRows, totalCols - 1, totalRows - 1, totalCols - 2)) && totalCols > 1) ProblemC(totalRows, totalCols - 1, alphabets, dpS, dpTS) solve

      if (!dpTS.contains(AlphabhetDpKey(alphabets, totalRows - 1, totalCols, totalRows - 2, totalCols - 1)) && totalRows > 1) ProblemC(totalRows - 1, totalCols, alphabets, dpS, dpTS) solve

      //alphabets > 1
      //S(k+1, (a,b)) = TS(k,a-1,n)- TS(k,n,b-1) + TS(k,a-1,b-1)
      Range(0, totalRows) foreach { i ⇒
        Range(0, totalCols) foreach { j ⇒
          dpS += AlphabhetDpKey(alphabets, totalRows, totalCols, i, j) → {
            if (i - 1 >= 0 && j - 1 >= 0) {
              dpTS(AlphabhetDpKey(alphabets - 1, totalRows, totalCols, i - 1, totalCols - 1)) - dpTS(AlphabhetDpKey(alphabets - 1, totalRows, totalCols, totalRows - 1, j - 1)) +
                dpTS(AlphabhetDpKey(alphabets - 1, totalRows, totalCols, i - 1, j - 1))
            }
            else if (i - 1 >= 0) {
              dpTS(AlphabhetDpKey(alphabets - 1, totalRows, totalCols, i - 1, totalCols - 1))
            }
            else if (j - 1 >= 0) {
              dpTS(AlphabhetDpKey(alphabets - 1, totalRows, totalCols, i - 1, totalCols - 1))
            }
            else {
              1
            }
          }
        }
      }
      dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, 0, 0)) = 1
      Range(0, totalRows) foreach { i ⇒
        Range(0, totalCols) foreach { j ⇒
          if (!dpTS.contains(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j))) {
            dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j)) = {
              if (i - 1 >= 0 && j - 1 >= 0) {
                (dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, i - 1, j)) + dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j - 1)) -
                  dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, i - 1, j - 1)) + dpS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j))) *
                  ProblemC(totalRows - i, j - 1, alphabets, dpS, dpTS).solveDirect * ProblemC(i - 1, totalCols - j, alphabets, dpS, dpTS).solveDirect

              }
              else if (j - 1 >= 0) {
                (dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j - 1)) + dpS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j))) *
                  ProblemC(totalRows, j - 1, alphabets, dpS, dpTS).solveDirect
              }
              else if (i - 1 >= 0) {
                (dpTS(AlphabhetDpKey(alphabets, totalRows, totalCols, i - 1, j)) + dpS(AlphabhetDpKey(alphabets, totalRows, totalCols, i, j))) *
                  ProblemC(i - 1, totalCols, alphabets, dpS, dpTS).solveDirect
              }
              else {
                1
              }
            }
          }
        }
      }
    }

  }
}

class ProblemCTesting extends FunSuite {
  test("a") {
    val x = ProblemC(2, 2, 2) solve

    println(x)

  }

}
