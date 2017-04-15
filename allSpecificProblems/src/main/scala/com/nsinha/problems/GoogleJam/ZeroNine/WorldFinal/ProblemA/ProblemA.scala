package com.nsinha.problems.GoogleJam.ZeroNine.WorldFinal.ProblemA

import java.lang.Integer.parseInt
import com.nsinha.common.Ratio
import scala.collection.mutable

/** Created by nsinha on 4/11/17.
  */

class ProblemA(inputs : String) {
  type Sum = Int
  type Day = Int
  type IntervalFromStart = Int
  type TournamentName = Int

  var totalDays = 0
  var totalTournaments = 0
  val tournamentsRounds = mutable.HashMap[TournamentName, (List[IntervalFromStart], Sum)]()
  val daysExpectation = mutable.HashMap[Day, Ratio]()
  var name = 1
  var maxSpanTournament : TournamentName = 1
  var dmax : IntervalFromStart = 0

  processInputs
  createDaysExpectation

  def getNewTournamentName : TournamentName = {
    name = name + 1
    name - 1
  }

  def createDaysExpectation = { Range(0, dmax) foreach { λ ⇒ daysExpectation += λ → findDayExpectation(λ) } }

  def findDayExpectation(day : Day) : Ratio = {
    val expects = tournamentsRounds map { case (key, (rounds, _)) ⇒ { rounds filter (λ ⇒ λ <= day) }.length }
    val squaredExpects = expects.foldLeft(0) { (Z, el) ⇒
      Z + expects.foldLeft(0)((ZZ, ell) ⇒ ell * el + ZZ)
    }
    new Ratio(squaredExpects, totalDays * totalDays)
  }

  def sumAllExpectations : Ratio = {
    val tillDmaxSum = daysExpectation.foldLeft(new Ratio(0, 1)){ (Z, el) ⇒ Z + (el._2) }
    tillDmaxSum + findDayExpectation(dmax) * (totalDays - dmax)
  }

  def processInputs = {
    val lines = inputs.split("\n")
    val firstLine = lines.head.split(" ")

    val res = {
      firstLine map {
        parseInt(_)
      }
    }.toSeq
    totalDays = res(0)
    totalTournaments = res(1)

    for (line ← lines.drop(1)) {
      val tournament = getNewTournamentName
      val allValues = line.split(" ") map {
        parseInt(_)
      }
      val rounds = allValues(0)

      val roundOffsets = {
        Array[Int](0) ++ allValues.drop(0)
      }.foldLeft(List[IntervalFromStart](), 0) { (Z, el) ⇒
        val offset = el + Z._2
        (Z._1 :+ (offset), offset)
      }

      tournamentsRounds += tournament → roundOffsets

      if (roundOffsets._2 > dmax) {
        dmax = roundOffsets._2
        maxSpanTournament = tournament
      }
    }
  }
}
