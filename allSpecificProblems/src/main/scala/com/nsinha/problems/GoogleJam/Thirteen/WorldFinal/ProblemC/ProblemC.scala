package com.nsinha.problems.GoogleJam.Thirteen.WorldFinal.ProblemC

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.util.Random

case class Coord(x: Double, y: Double)
/**
  * Created by nishchaysinha on 9/2/17.
  */
class ProblemC {

  val epsilon = 1e-11

  def solve(l: List[Coord]) = {

    val median = getMedian(l)
    val map = mutable.HashMap[Double, Coord]()

    val allAngles = perturb(l)  map {x =>
      val angle = (findAngle(median, x))
      map += angle -> x
      angle
    }


    val solnOpt = findSoln(allAngles)
    solnOpt match {
      case None =>
        println(solnOpt)
      case Some(soln) =>
        println(map(soln._2(soln._1)))
    }

  }
  def perturb(coords: List[Coord]): List[Coord] = {
    val r = new Random(System.currentTimeMillis())
    coords map (x => Coord(x.x + r.nextDouble()*epsilon ,x.y + r.nextDouble()*epsilon))
    coords
  }


  def findAngle(coord: Coord, coord1: Coord): Double = {
    val ydiff = (coord1.y - coord.y + epsilon)
    val xdiff = (coord1.x - coord.x + epsilon)

    val absangle = if(math.abs(xdiff) > 3* epsilon) math.tan(math.abs(ydiff/xdiff)) else math.Pi/2

    if (ydiff >0 && xdiff > 0) {
      absangle
    } else if (ydiff < 0 && xdiff >0 ) {
      2*math.Pi -absangle
    } else if (ydiff < 0 && xdiff <0 ) {
      math.Pi + absangle
    } else {
      math.Pi - absangle
    }
  }


  def findSoln (angles: List[Double]): Option[(Int, Array[Double])] = {
    val sortedAngles = angles.sorted.toArray //nlogn
    val size = sortedAngles.size

    val sortedAnglesInQue = sortedAngles   .foldLeft(mutable.Queue[Double]()) {(Z, el) => Z += el }

    for (iter <- Range(0, size/4)) {

      val angle0 = sortedAnglesInQue.dequeue()

      //move points from bin1 to bin0
      val indx1 = findOffsetInBin(sortedAngles, angle0, math.Pi/2, iter)
      val indx2 = findOffsetInBin(sortedAngles, angle0, math.Pi, iter)
      val indx3 = findOffsetInBin(sortedAngles, angle0, 3*math.Pi/2, iter)
      val gaps = Array(indx1 , indx2 - indx1  , indx3 -indx2 )
      val alltrue = gaps.forall(x => x == size/4)
      if (alltrue) {
        return Option((iter,sortedAngles))
      }

    }
    None
  }



  def findOffsetInBin(angles: Array[Double], angle: Double, distance: Double, start: Int): Int = {
    var begin = start
    var end = angles.size - 1
    var cond = false
    while (true) {
      var mid = (begin + end)/2
      if (angles(mid) <= (angle + distance)) {
        begin = mid
      } else {
        end = mid
      }
      if (end -begin < 2) {
        if (angles(end) <= angle + distance) return end else return begin

      }

    }

    return 0

  }


  def getMedian(coords: List[Coord]): Coord = {
    val (xT, yT) = coords.foldLeft((0d,0d)) { (Z , el) => (Z._1 + el.x, Z._2 + el.y) }
    Coord(xT/coords.size, yT/coords.size)
  }


}



class ProblemCTesting extends FunSuite {

  test("1") {
    val c = {new ProblemC}.solve(List((Coord(1,0)),Coord(-1,0), Coord(0,1),Coord(0,-1)))
  }


}

