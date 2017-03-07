package com.nsinha.problems.ConvexHull

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/3/17.
  */
case class Coord(x : Double, y : Double)
class ConvexHull(inputs : List[Coord]) {

  case class CoordWithAngle(coord : Coord, angle : Double)

  val MINISCULEPOSITIVE = 1e-11

  def findAngelBetweenThreePoints(x : Coord, y : Coord, z : Coord) : Double = {
    //yz angle with x axis
    //xy angel with x axis
    //diff is the angle
    math.atan((z.y - y.y) / (z.x - y.x)) - math.atan((x.y - y.y) / (x.x - y.x))
  }

  def isAcute(x : Coord, y : Coord, z : Coord) : Boolean = {
    val res = findAngelBetweenThreePoints(x, y, z)

    if (res > 0) true else false
  }

  def findAngle(src : Coord, dest : Coord) : Double = {
    val xOffset = dest.x - src.x + MINISCULEPOSITIVE
    val yOffset = dest.y - src.y + MINISCULEPOSITIVE

    val signXOffset = if (xOffset >= 0) true else false
    val signYOffset = if (yOffset >= 0) true else false

    val absAngleQuantity = math.atan(math.abs(yOffset / xOffset))

    (signXOffset, signYOffset) match {
      case (true, true)   ⇒ absAngleQuantity
      case (true, false)  ⇒ 2 * math.Pi - absAngleQuantity
      case (false, true)  ⇒ math.Pi - absAngleQuantity
      case (false, false) ⇒ math.Pi + absAngleQuantity
    }
  }

  def getHull : List[Coord] = {
    val pivot = inputs.maxBy(_.x)
    var que = mutable.MutableList[Coord]()
    que += pivot

    val listOfInputsWithPivotAngle = inputs filter (x ⇒ x != pivot) map { x ⇒ CoordWithAngle(x, findAngle(pivot, x)) } //o(n)

    var sortedByAngleInputs = listOfInputsWithPivotAngle.sortBy(x ⇒ x.angle) //o(nlogn)

    while (sortedByAngleInputs.nonEmpty) {
      val top = sortedByAngleInputs.head
      val allWithSameAngles = sortedByAngleInputs filter (x ⇒ x.angle == top.angle)
      sortedByAngleInputs = sortedByAngleInputs filter (x ⇒ x.angle != top.angle)
      val sameAngleSorted = allWithSameAngles.sortBy(_.coord.y)
      //we only need to think of this
      val thisAngleHead = sameAngleSorted.head

      //check last two elements from que.if acute angle remove the middle one. and keep checking
      var cond = true
      while (cond) {
        if (que.size < 2) {
          que += thisAngleHead.coord
          cond = false
        }
        else {
          val twoElems = {
            que.takeRight(2)
          }
          val middle = twoElems(1)
          val end = twoElems(0)

          if (!isAcute(thisAngleHead.coord, middle, end)) {
            que = que.dropRight(1)
            cond = true
          }
          else {
            que += thisAngleHead.coord
            cond = false
          }
        }
      }
    }

    que.toList
  }
}

class Testing extends FunSuite {

  test("a") {
    val inputs = List(Coord(0, 0), Coord(1, 0), Coord(1, 1))

    val p = new ConvexHull(inputs)

    print(p.getHull)

  }

  test("the acute angle fn") {

    val inputs = List(Coord(1, 0), Coord(-1, 0), Coord(0, -1), Coord(0, 1), Coord(0.5, 0), Coord(-0.5, 0))
    val p = new ConvexHull(inputs)

    println(p.findAngelBetweenThreePoints(Coord(0, 0), Coord(0.1, -0.1), Coord(2, 0)))
    println(p.isAcute(Coord(0, 0), Coord(0.1, -0.1), Coord(2, 0)))
    println(p.findAngelBetweenThreePoints(Coord(0, 0), Coord(0.1, 0.1), Coord(2, 0)))
    println(p.isAcute(Coord(0, 0), Coord(0.1, 0.1), Coord(2, 0)))
    println(p.findAngelBetweenThreePoints(Coord(0, 0), Coord(1, 0.001), Coord(2, 0)))
    println(p.isAcute(Coord(0, 0), Coord(1, 0.001), Coord(2, 0)))
    println(p.findAngelBetweenThreePoints(Coord(0, 0), Coord(1, -0.001), Coord(2, 0)))
    println(p.isAcute(Coord(0, 0), Coord(1, -0.001), Coord(2, 0)))
    println(p.findAngelBetweenThreePoints(Coord(0, 0), Coord(1, 0), Coord(2, 0)))
    println(p.isAcute(Coord(0, 0), Coord(1, 0), Coord(2, 0)))

    println(p.findAngelBetweenThreePoints(Coord(1, 0), Coord(0, 1), Coord(-0.5, 0)))
    println(p.isAcute(Coord(1, 0), Coord(0, 1), Coord(-0.5, 0)))

    println(p.findAngelBetweenThreePoints(Coord(-0.5, 0), Coord(0, 1), Coord(1, 0)))
    println(p.isAcute(Coord(-0.5, 0), Coord(0, 1), Coord(1, 0)))
    println(p.getHull)
  }

}
