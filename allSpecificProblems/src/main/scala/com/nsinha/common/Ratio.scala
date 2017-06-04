package com.nsinha.common

/** Created by nsinha on 4/12/17.
  */

class Ratio(numIn : Int, denumIn : Int) {

  val (num, denum) = normalize

  def +(that : Ratio) : Ratio = {
    if (denum == that.denum) return new Ratio(that.num + num, denum)

    val r = new Ratio(that.num * denum + num * that.denum, denum * that.denum)
    r
  }

  def *(that : Int) : Ratio = new Ratio(num * that, denum)
  def *(that : Ratio) : Ratio = new Ratio(num * that.num, denum * that.denum)
  def /(that : Ratio) : Ratio = new Ratio(num * that.denum, denum * that.num)

  def normalize = {
    val gcd = findGcd(numIn, denumIn)
    (numIn / gcd, denumIn / gcd)
  }

  def findGcd(x : Int, y : Int) : Int = {
    if (x == 0 || y == 0) return 1
    var res = 1
    val (less, greater) = if (x > y) (y, x) else (x, y)
    val quot = greater / less
    val rem = greater - quot * less
    if (rem == 0) return less
    findGcd(rem, less)
  }

}
