package com.nsinha.problems.GoogleJam.Twelve.WorldFinal.ProblemA

import com.nsinha.common.Coordinate
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 5/23/17.
  */

case class Event(time : Long, pos : Coordinate)

object Event {
  implicit def eventOrdering : Ordering[Event] = {
    new Ordering[Event] {
      override def compare(cur : Event, that : Event) : Int = {
        if (cur.time < that.time) return -1
        if (cur.time > that.time) return 1
        0
      }
    }
  }

  implicit def stateOrdering : Ordering[StateAtTimePoint] = {
    new Ordering[StateAtTimePoint] {
      override def compare(cur : StateAtTimePoint, that : StateAtTimePoint) : Int = {
        if (cur.time < that.time) return 1
        if (cur.time > that.time) return -1
        0
      }
    }
  }

}

case class StateAtTimePoint(time : Long, strikeTime : Long, cnt : Int, pos : Coordinate = Coordinate(0, 0), hits : List[Int] = List())

case class ProblemA(eventsList : List[Event]) {

  import Event._

  val rechargeTime = 750
  val cellTravelTime = 100
  val eventDuration = 1000
  val mapOfEvents = createEventMap

  val coordinateHashMap = mutable.HashMap[(Coordinate, Int), Long]()
  val priorityQueue = mutable.PriorityQueue[StateAtTimePoint]()
  priorityQueue += StateAtTimePoint(0, -750, 0)
  var maxCnt = 0
  processQue
  println(maxCnt)

  def createEventMap = {
    val hashMap = mutable.TreeMap[Int, Event]()
    var cnt = 0
    eventsList foreach { ev ⇒
      hashMap += cnt → ev
      cnt = cnt + 1
    }
    hashMap
  }

  def processQue = {
    while (priorityQueue.nonEmpty) {
      val curState = priorityQueue.dequeue()
      val curCoordinate = curState.pos
      val curTime = curState.time
      val curStrikeTime = curState.strikeTime
      val curCnt = curState.cnt
      val hits = curState.hits
      if (!coordinateHashMap.contains((curCoordinate, curCnt))) {
        //ignoring later time with same cnt value
        coordinateHashMap += (curCoordinate, curCnt) → curStrikeTime
        val allReachableEvents = findAllReachableEvents(curCoordinate, curTime, curStrikeTime, hits)
        allReachableEvents foreach (x ⇒ priorityQueue.enqueue(x))
      }
    }

  }

  def calcTravelTime(pos1 : Coordinate, pos2 : Coordinate) : Long = {
    import math._
    max(abs(pos1.x - pos2.x), abs(pos1.y - pos2.y)) * cellTravelTime
  }

  def actualShootingTime(earliestTimeOfShoot : Long, timeOfEventStart : Long) : Option[Long] = {

    if (earliestTimeOfShoot <= timeOfEventStart) return Option(timeOfEventStart)

    if (earliestTimeOfShoot > (timeOfEventStart + eventDuration)) return None

    Option(earliestTimeOfShoot)

  }

  def findAllReachableEvents(curPos : Coordinate, curTime : Long, lastStrikeTime : Long, hits : List[Int]) : List[StateAtTimePoint] = {
    import math._
    val candidateEvents = mapOfEvents.keys.toSet.diff(hits.toSet)
    val mutableListOfNewStates = mutable.MutableList[StateAtTimePoint]()

    val earliestTimeForNextShoot = lastStrikeTime + rechargeTime

    for (candidate ← candidateEvents) {
      val travelTime = calcTravelTime(curPos, mapOfEvents(candidate).pos)
      val earliestReachTime = curTime + travelTime
      val earliestShootTime = max(earliestTimeForNextShoot, earliestReachTime)
      Option(actualShootingTime(earliestShootTime, mapOfEvents(candidate).time)) foreach { x ⇒
        if (x != None) {
          val curStriketime = x.get
          mutableListOfNewStates += StateAtTimePoint(curStriketime, curStriketime, hits.size + 1, mapOfEvents(candidate).pos, hits :+ candidate)
          if (hits.size + 1 > maxCnt) maxCnt = hits.size + 1
        }
      }
    }

    mutableListOfNewStates.toList
  }
}

class ProblemATesting extends FunSuite {

  test("a") {
    val events = List(Event(0, Coordinate(-1, 0)), Event(0, Coordinate(1, 0)), Event(1000, Coordinate(10, -10)), Event(1000, Coordinate(10, 10)))
    ProblemA(events)
  }
  test("b") {
    val events = List(Event(0, Coordinate(1, 1)), Event(0, Coordinate(2, 2)), Event(0, Coordinate(3, 3)))
    ProblemA(events)
  }
  test("c") {
    val events = List(Event(1000, Coordinate(10, -10)), Event(1000, Coordinate(-10, 10)), Event(1000, Coordinate(10, -10)), Event(
      1000,
      Coordinate(-10, -10)
    ), Event(2000, Coordinate(20, 20)))
    ProblemA(events)
  }

}
