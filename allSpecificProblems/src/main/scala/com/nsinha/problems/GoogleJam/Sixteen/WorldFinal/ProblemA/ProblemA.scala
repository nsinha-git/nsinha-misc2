package com.nsinha.problems.GoogleJam.Sixteen.WorldFinal.ProblemA

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 5/19/17.
  */

trait Token {
  def getValue : Char
  def setValue(x : Char)
}
trait ConnectorToken extends Token

object CantBeDetermined extends ConnectorToken {
  override def getValue = '-'
  override def setValue(x : Char) = ???
}

object Or extends ConnectorToken {
  override def getValue = '|'
  override def setValue(x : Char) = ???
}

object And extends ConnectorToken {
  override def getValue = ' '
  override def setValue(x : Char) = ???
}

object Star extends Token {
  override def getValue = '*'
  override def setValue(x : Char) = ???
}

object BracketLeft extends Token {
  override def getValue = '('
  override def setValue(x : Char) = ???
}

object BracketRight extends Token {
  override def getValue = ')'
  override def setValue(x : Char) = ???
}
object ErrorToken extends Token {
  override def getValue = '.'
  override def setValue(x : Char) = ???
}

case class Digit(y : Char) extends Token {
  var x = 0
  setValue(y)
  override def getValue = x.toChar
  override def setValue(i : Char) = { x = i.toInt }
}
case class NumberRange(low : Long, high : Long)

case class Expression(e : String, star : Boolean = false) {
  def inRange(x : Int, r : NumberRange) : Boolean = {
    if (x >= r.low && x <= r.high) true
    false
  }

  val atoms = mutable.MutableList[Expression]()
  var valueOpt : Option[Int] = None
  var internalConnectorsOpt : Option[ConnectorToken] = None

  splitAlongAtoms
  if (internalConnectorsOpt == None) internalConnectorsOpt = Option(And)

  def updateNextConnector(nextConnector : Token) = {
    if (nextConnector == CantBeDetermined) {
      internalConnectorsOpt match {
        case None                       ⇒ internalConnectorsOpt = Option(And)
        case Some(internalConnectorOpt) ⇒
      }
    }
    else {
      internalConnectorsOpt match {
        case None                    ⇒ internalConnectorsOpt = if (nextConnector == Or) Option(Or) else Option(And)
        case Some(internalConnector) ⇒ assert(internalConnector == nextConnector)
      }
    }

  }

  def splitAlongAtoms : Unit = {
    if (e.size == 1) {
      valueOpt = Option(Integer.parseInt(e))
      return
    }
    var cond = true
    var parseString = e
    while (cond) {
      if (parseString.charAt(0) == '(') {
        val (atomicString, nextparseString, isStar, nextConnector) = Common.getFullyBracedExpression(parseString)
        updateNextConnector(nextConnector)
        atoms += Expression(Common.removeLeftRightBrace(atomicString), isStar)
        parseString = nextparseString
      }
      else {
        //this must be a digit(s) followed by a connector
        if (parseString.size > 1) {
          val t = Common.firstNonDigitIndexOrEnd(parseString)
          if (t < parseString.size) {
            val nextConnector = Common.getConnectorToken(parseString(t))
            if (nextConnector == CantBeDetermined) {
              updateNextConnector(And)
              atoms += Expression(parseString.substring(0, t))
              internalConnectorsOpt match {
                case Some(And) ⇒
                  parseString = parseString.substring(1)
                case Some(Or) ⇒
                  parseString = parseString.substring(2)
                case None ⇒ assert(false)
              }
            }
            else {
              updateNextConnector(nextConnector)
              atoms += Expression(parseString.substring(0, t))
              parseString = parseString.substring(t + 1)
            }
          }
          else {
            //t= end of string
            if (parseString == e) {
              for (i ← Range(0, t)) atoms += Expression(parseString.charAt(i).toString)
            }
            else {
              atoms += Expression(parseString)
            }
            parseString = ""
          }
        }
        else {
          //last digit
          atoms += Expression(parseString.charAt(0).toString)
          internalConnectorsOpt match {
            case None ⇒ internalConnectorsOpt = Option(And)
            case _    ⇒
          }
          cond = false
          parseString = ""
        }
      }
      if (parseString.size == 0) cond = false
    }
  }

  def getLowestValueFromOrExpOfLength(l : Int) = {
  }

  def getHighestValue(totalLen : Int, curLen : Int = 0, alreadySeenLen : Int = 0) : (Int, Long) = {
    getValue(totalLen : Int, curLen : Int, alreadySeenLen : Int, true)
  }

  def getLowestValue(totalLen : Int, curLen : Int = 0, alreadySeenLen : Int = 0) : (Int, Long) = {
    getValue(totalLen : Int, curLen : Int, alreadySeenLen : Int, false)
  }

  def getValue(totalLen : Int, curLen : Int, alreadySeenLen : Int, highest : Boolean) : (Int, Long) = {
    val nonZeroSeen = alreadySeenLen > 0
    if (false) {
      for (expectedTotalLen ← Range(alreadySeenLen + 1, totalLen + 1)) {
        val expectedLen = expectedTotalLen - alreadySeenLen

        val lowestOpt : Option[Long] = None //getLowestValueFromOrExpOfLength(expectedLen)
        val highestOpt = None //getHighestValueFromOrExpOfLength(expectedLen)
        lowestOpt match {
          case None ⇒
          case Some(lowest) ⇒
            val highest = highestOpt.get
            if (highest == lowest) {
              //and exp
              return (alreadySeenLen + expectedLen, lowest)

            }
            else {
              if (highest) {
                return (alreadySeenLen + expectedLen, highest)
              }
              else {
                return (alreadySeenLen + expectedLen, lowest)
              }
            }

        }

      }

    }
    (0, 0)
  }
}

object Common {

  def removeLeftRightBrace(s : String) : String = {
    val s1 = s drop 1
    //from end remove the brace and connector if present
    if (s1.charAt(s1.length - 1) != ')') {
      s1.dropRight(2)
    }
    else {
      s1.dropRight(1)
    }
  }
  // val (atomicString, nextparseString, isStar, nextConnector) = Common.getFullyBracedExpression(parseString)
  def getFullyBracedExpression(s : String) : (String, String, Boolean, Token) = {
    //keep Count of open brace:
    var cnt = 0
    var cond = true
    var i = 0
    while (cond) {
      if (s.charAt(i) == '(') cnt = cnt + 1
      if (s.charAt(i) == ')') cnt = cnt - 1
      i = i + 1
      if (cnt == 0) cond = false
    }

    // i may point to end of String or * or (
    if (i == s.length) { //end Of String
      (s.substring(0, i), "", false, CantBeDetermined)
    }
    else if (i <= s.length - 2) { //there are 2 chars after this
      val ithToken = Common.getToken(s(i))
      val iNextToken = Common.getConnectorToken(s(i + 1))
      if (ithToken == Star) {
        if (iNextToken == CantBeDetermined) {
          (s.substring(0, i), s.substring(i + 1), true, CantBeDetermined)
        }
        else {
          if (iNextToken == Or) {
            (s.substring(0, i), s.substring(i + 2), true, iNextToken)
          }
          else {
            (s.substring(0, i), s.substring(i + 1), true, iNextToken)
          }
        }
      }
      else {
        if (ithToken == Or) {
          (s.substring(0, i), s.substring(i + 1), false, Or)
        }
        else {
          (s.substring(0, i), s.substring(i), false, And)
        }
      }
    }
    else { //there is 1 character after this better be *
      val ithToken = Common.getToken(s(i))
      if (ithToken == Star) {
        (s.substring(0, i), "", true, CantBeDetermined)
      }
      else {
        assert(false, "last token is somehow not star")
        null
      }
    }
  }

  def getToken(c : Char) : Token = {
    if (c <= 9.toChar && c >= 0.toChar) return Digit(c)
    if (c == '(') return BracketLeft
    if (c == ')') return BracketRight
    if (c == '|') return Or
    if (c == '*') return Star
    return And
  }

  def getConnectorToken(c : Char) : Token = {
    if (c <= 9.toChar && c >= 0.toChar) return And
    if (c == '(') return CantBeDetermined
    if (c == ')') return CantBeDetermined
    if (c == '|') return Or
    return And
  }

  def firstNonDigitIndexOrEnd(s : String) : Int = {
    var indx = -1
    var cnt = 0
    for (c ← s) {
      if (c >= '0' & c <= '9') {

      }
      else {
        if (indx == -1) indx = cnt
      }
      cnt = cnt + 1
    }
    if (indx == -1) indx = s.length
    indx
  }

  //1(56|(((7|8))*9)*)

  def getAllValuesForExpression(e : Expression, maxLen : Int) : Long = {
    var sum : Long = 0

    for (i ← Range(1, maxLen + 1)) {
      val high = e.getHighestValue(i)
      val low = e.getLowestValue(i)
      if (low == high) {
        sum += 1
      }
      else {
        val list : List[Long] = Nil //e.produceAllOrElementsInLeastForm()
        val isZeroPresent = !(list forall (x ⇒ x != 0))
        sum += (list.length - 1) * math.pow(list.length, i - 1).toLong

      }
    }
    0

  }

}

case class ProblemA(r : NumberRange, expressionString : String) {

  val exp = Expression(expressionString, false)
}

class ProblemATesting extends FunSuite {

  test("a") {
    val problemA = ProblemA(NumberRange(1, math.pow(10, 18).toLong), "(0)*1(0)*")
    println(problemA)
    val problemB = ProblemA(NumberRange(1, math.pow(10, 18).toLong), "((0|1))*")
    println(problemB)
  }
  test("b") {
    val problemC = ProblemA(NumberRange(1, math.pow(10, 18).toLong), "(01|23|45|67|23)")
    println(problemC)
    val problemD = ProblemA(NumberRange(1, math.pow(10, 18).toLong), "(12)*(34)*")
    println(problemD)
    val problemE = ProblemA(NumberRange(1, math.pow(10, 18).toLong), "((0|1|2|3|4|5|6|7|8|9))*")
    println(problemE)
    val problemF = ProblemA(NumberRange(1, math.pow(10, 18).toLong), "1(56|(((7|8))*9)*)")
    println(problemF)
  }

}
