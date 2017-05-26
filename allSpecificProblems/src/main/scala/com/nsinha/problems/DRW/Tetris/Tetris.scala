package com.nsinha.problems.DRW.Tetris

import scala.collection.mutable

/** Created by nsinha on 5/25/17.
  */
/** *
  * 1. A tetris model is a 3x4 array with the blocks filled according to various models of tetris game.We have Q,S ... models
  * 2. A tetris model also contains bottomest row when seen from down on a give column. This is precomputed and will help in running the code faster.
  * 3. There is a game board represented in BoardState. This conatins of a bottommost row at key -1. This is a psuedo row and
  * no tetris will fall below this row.
  * 4. When a tetris model appears, we calculate the base row according to height and bottom row  for the given column in that model and
  * the board  layout; the base row is where the tetris will stand. The height is maximum differnce of stopping point on board(see 5) of a
  * column  and height of  that column in model across all columns inside the model (at most 4)
  * 5. We maintain a columnwise highest row taken data on tetris board to  correctly do the operation at 4.
  * 6. When a row of board is completely removed, we shift rows. This is the most cpu intensive ops of compelxity (10*NumberOf rows present)
  * As number of rows in a game is less than 100, the worst case complexity in game is o(1000).
  * 7. TetrisTesting has the way of showing how to test a passed string.
  *
  *
  */
/*
Result for the file
4
0
2
4
1
0
2
2
2
1
1
4
3
1
3
3
8
8
0
3
 */
object Common {
  val MAX_ROWS_TETRIS_MODEL = 3
  val MAX_COLS_IN_TERTRIS_MODEL = 4
  val MAX_COLS_IN_BOARD = 10
}

import Common._
trait TetrisModel {
  val arrayFilled : Array[Array[Int]] = new Array[Array[Int]](MAX_ROWS_TETRIS_MODEL)

  def update(a0 : Array[Int], a1 : Array[Int], a2 : Array[Int]) : Unit = {
    a0 ensuring (a0.size == MAX_COLS_IN_TERTRIS_MODEL)
    a1 ensuring (a1.size == MAX_COLS_IN_TERTRIS_MODEL)
    a2 ensuring (a2.size == MAX_COLS_IN_TERTRIS_MODEL)
    arrayFilled.update(0, a0)
    arrayFilled.update(1, a1)
    arrayFilled.update(2, a2)
  }
  var bottomLeftCol : Int
  var topLeftCol : Int
  var topRow : Int
  var col0BottomestRow : Int
  var col1BottomestRow : Int
  var col2BottomestRow : Int
  var col3BottomestRow : Int
}

object Q extends TetrisModel {
  val a0 = Array[Int](1, 1, 0, 0)
  val a1 = Array[Int](1, 1, 0, 0)
  val a2 = Array[Int](0, 0, 0, 0)
  override var topRow = 1
  override var topLeftCol = 0
  override var bottomLeftCol = 0
  override var col0BottomestRow : Int = 0
  override var col1BottomestRow : Int = 0
  override var col2BottomestRow : Int = -1
  override var col3BottomestRow : Int = -1
  update(a0, a1, a2)
}

object Z extends TetrisModel {
  val a0 = Array[Int](0, 1, 1, 0)
  val a1 = Array[Int](1, 1, 0, 0)
  val a2 = Array[Int](0, 0, 0, 0)
  override var topRow = 1
  override var topLeftCol = 0
  override var bottomLeftCol = 1
  override var col0BottomestRow : Int = 1
  override var col1BottomestRow : Int = 0
  override var col2BottomestRow : Int = 0
  override var col3BottomestRow : Int = -1
  update(a0, a1, a2)
}
object S extends TetrisModel {
  val a0 = Array[Int](1, 1, 0, 0)
  val a1 = Array[Int](0, 1, 1, 0)
  val a2 = Array[Int](0, 0, 0, 0)
  override var topRow = 1
  override var topLeftCol = 1
  override var bottomLeftCol = 0
  override var col0BottomestRow : Int = 0
  override var col1BottomestRow : Int = 0
  override var col2BottomestRow : Int = 1
  override var col3BottomestRow : Int = -1
  update(a0, a1, a2)
}
object T extends TetrisModel {
  val a0 = Array[Int](0, 1, 0, 0)
  val a1 = Array[Int](1, 1, 1, 0)
  val a2 = Array[Int](0, 0, 0, 0)
  override var topRow = 1
  override var topLeftCol = 0
  override var bottomLeftCol = 1
  override var col0BottomestRow : Int = 1
  override var col1BottomestRow : Int = 0
  override var col2BottomestRow : Int = 1
  override var col3BottomestRow : Int = -1
  update(a0, a1, a2)
}

object I extends TetrisModel {
  val a0 = Array[Int](1, 1, 1, 1)
  val a1 = Array[Int](0, 0, 0, 0)
  val a2 = Array[Int](0, 0, 0, 0)
  override var topRow = 0
  override var topLeftCol = 0
  override var bottomLeftCol = 0
  override var col0BottomestRow : Int = 0
  override var col1BottomestRow : Int = 0
  override var col2BottomestRow : Int = 0
  override var col3BottomestRow : Int = 0
  update(a0, a1, a2)
}

object L extends TetrisModel {
  val a0 = Array[Int](1, 1, 0, 0)
  val a1 = Array[Int](1, 0, 0, 0)
  val a2 = Array[Int](1, 0, 0, 0)
  override var topRow = 2
  override var topLeftCol = 0
  override var bottomLeftCol = 0
  override var col0BottomestRow : Int = 0
  override var col1BottomestRow : Int = 0
  override var col2BottomestRow : Int = -1
  override var col3BottomestRow : Int = -1
  update(a0, a1, a2)
}

object J extends TetrisModel {
  val a0 = Array[Int](1, 1, 0, 0)
  val a1 = Array[Int](0, 1, 0, 0)
  val a2 = Array[Int](0, 1, 0, 0)
  override var topRow = 2
  override var topLeftCol = 1
  override var bottomLeftCol = 0
  override var col0BottomestRow : Int = 0
  override var col1BottomestRow : Int = 0
  override var col2BottomestRow : Int = -1
  override var col3BottomestRow : Int = -1
  update(a0, a1, a2)
}

case class LeftShiftedModel(tetrisModel : TetrisModel, left : Int) {
  private var bottomRow = -1
  def setBottomRow(row : Int) = { bottomRow = row }
  def getBottomRow = bottomRow

  def getBlocksAt(r : Int) = {
    tetrisModel.arrayFilled(r)
  }

  def effectiveCols : List[(Int, Int)] = {
    val list = mutable.MutableList[(Int, Int)]()
    //from Tetris piece find all the cols visible from bottom and their elevation
    if (tetrisModel.col0BottomestRow != -1)
      list += ((0 + left, tetrisModel.col0BottomestRow))
    if (tetrisModel.col1BottomestRow != -1)
      list += ((1 + left, tetrisModel.col1BottomestRow))
    if (tetrisModel.col2BottomestRow != -1)
      list += ((2 + left, tetrisModel.col2BottomestRow))
    if (tetrisModel.col3BottomestRow != -1)
      list += ((3 + left, tetrisModel.col3BottomestRow))
    list toList
  }
}

class BoardState {
  private val stopCols = createStopCols
  private var mapOfRows = createMapOfBoardRows
  val debugOfLogOfModels = mutable.MutableList[(TetrisModel, Int)]()

  def createMapOfBoardRows = {
    val mp = mutable.HashMap[Int, Array[Int]]()
    val arr = new Array[Int](MAX_COLS_IN_BOARD)
    for (i ← Range(0, MAX_COLS_IN_BOARD)) {
      arr.update(i, 1)
    }
    mp += -1 → arr
  }

  def height = mapOfRows.size - 1

  def getBottomestRowForCurrentModel(curBottomRow : Int, elevationOfBlock : Int, stopBaseOfThisBlock : Int) : Int = {
    val candidateBottom = stopBaseOfThisBlock - elevationOfBlock
    if (candidateBottom > curBottomRow) candidateBottom else curBottomRow
  }

  def update(piece : TetrisModel, leftCol : Int) : Int = {
    debugOfLogOfModels += ((piece, leftCol))
    val leftShiftedModel = LeftShiftedModel(piece, leftCol)

    val effectiveColsWithElevation : List[(Int, Int)] = leftShiftedModel.effectiveCols
    var bottomestRow = -1
    effectiveColsWithElevation foreach {
      case (col, ele) ⇒
        val rowNo : Int = findStopBoardRowForCol(col)
        bottomestRow = getBottomestRowForCurrentModel(bottomestRow, ele, rowNo)
    }
    //now we have a correct bottomestRow for this piece
    leftShiftedModel.setBottomRow(bottomestRow)
    fixBoardState(leftShiftedModel)
    height
  }

  def findStopBoardRowForCol(col : Int) : Int = {
    stopCols(col) + 1
  }

  def createStopCols : mutable.Map[Int, Int] = {
    val mp = mutable.Map[Int, Int] ()
    for (i ← Range(0, MAX_COLS_IN_BOARD)) {
      mp += (i → -1)
    }
    mp
  }

  def updateStopCols(row : Int, col : Int) = {
    if (col != -1) {
      stopCols += col → row
    }
    else {
      comprehensiveUpdateStopCols(row)
    }
  }

  def shiftMapCols(row : Int) = {
    val shiftedmapOfRows = createMapOfBoardRows

    mapOfRows foreach {
      case (k, v) ⇒
        shiftedmapOfRows += { if (k > row) (k - 1) → v else k → v }
    }
    mapOfRows = shiftedmapOfRows
  }

  def comprehensiveUpdateStopCols(row : Int) : Unit = {
    shiftMapCols(row)
    //find all cols that ve this as row
    val allcols = Range(0, MAX_COLS_IN_BOARD).toList

    allcols foreach { col ⇒
      val row = findHighestRowFilledForCol(col)
      stopCols += col → row
    }
  }

  def findHighestRowFilledForCol(col : Int) : Int = {
    var higestrow = -1
    mapOfRows foreach {
      case (rowNo, rowContents) ⇒
        if (rowContents(col) == 1) { if (rowNo > higestrow) higestrow = rowNo }
    }
    higestrow
  }

  def fixBoardState(leftShiftedModel : LeftShiftedModel) = {
    val tobeRemovedRows = mutable.MutableList[Int]()
    val bottomRow : Int = leftShiftedModel.getBottomRow
    val leftShift = leftShiftedModel.left
    for (relativeRow ← Range(0, MAX_ROWS_TETRIS_MODEL)) {
      val rowArray : Array[Int] = leftShiftedModel.getBlocksAt(relativeRow)
      val tobeRemovedRow = fixBoardRowPos(bottomRow + relativeRow, rowArray, leftShift)
      if (tobeRemovedRow >= 0) tobeRemovedRows += tobeRemovedRow
    }
    tobeRemovedRows foreach { k ⇒
      mapOfRows.-=(k)
    }

    tobeRemovedRows foreach { k ⇒ updateStopCols(k, -1) }
  }

  def fixBoardRowPos(rowAffected : Int, filledValue : Array[Int], shiftCol : Int) : Int = {
    if (!filledValue.forall(x ⇒ x == 0)) {
      if (!mapOfRows.contains(rowAffected)) {
        mapOfRows += rowAffected → {
          val x = new Array[Int](MAX_COLS_IN_BOARD)
          for (i ← Range(0, MAX_COLS_IN_BOARD)) {
            x.update(i, 0)
          }
          x
        }
      }

      val boardRow = mapOfRows(rowAffected)

      filledValue zip Range(0, MAX_COLS_IN_TERTRIS_MODEL) foreach {
        case (filled, relativeCol) ⇒
          if (filled == 1) {
            val prevState = boardRow(relativeCol + shiftCol)
            prevState ensuring (prevState == 0)
            boardRow.update(relativeCol + shiftCol, 1)
            //at this point update Stop cols too
            updateStopCols(rowAffected, relativeCol + shiftCol)
          }
      }
      if (boardRow.forall(x ⇒ x == 1)) rowAffected else -1
    }
    else {
      -1
    }

  }

  def debugPrintBoardState = {
    val rowsSorted = mapOfRows.toList.sortBy(x ⇒ x._1).reverse
    rowsSorted foreach {
      case (rowNo, rowContents) ⇒
        print(s"rowNo${rowNo % 10} ")
        for (el ← rowContents) {
          print(el)
        }
        println()
    }
    println(s"height: $height")
    println(debugOfLogOfModels)
    println(stopCols.toList.sorted)
  }
}

object Tetris {

  def getTetrisModelFromString(s : String) : TetrisModel = {
    val sLowerCase = s.toLowerCase
    if (sLowerCase == "q") return Q
    if (sLowerCase == "z") return Z
    if (sLowerCase == "s") return S
    if (sLowerCase == "t") return T
    if (sLowerCase == "i") return I
    if (sLowerCase == "l") return L
    if (sLowerCase == "j") return J
    assert(false, "Should not have comedown here")
    null
  }

}

case class Tetris(stringOfModel : String, debug : Boolean = false) {
  val listOfModels : List[(String, Int)] = {
    stringOfModel.split(",").foldLeft(List[(String, Int)]()) {
      (Z, el) ⇒ Z :+ ((el.charAt(0).toString, Integer.parseInt(el.charAt(1).toString)))
    }
  }
  val boardState = new BoardState
  if (debug) boardState.debugPrintBoardState
  listOfModels foreach { curPieceWithPos ⇒
    val curPiece = Tetris.getTetrisModelFromString(curPieceWithPos._1)
    boardState.update(curPiece, curPieceWithPos._2)
    if (debug) boardState.debugPrintBoardState
  }
  println(boardState.height)
}

object TetrisTesting extends App {
  val testString = ("Q0,Q1\nQ0,Q2,Q4,Q6,Q8\nQ0,Q2,Q4,Q6,Q8,Q1\nQ0,Q2,Q4,Q6,Q8,Q1,Q1\nI0,I4,Q8\nI0,I4,Q8,I0,I4\nL0,J2,L4,J6,Q8\nL0,Z1,Z3,Z5,Z7\nT0,T3\nT0,T3,I6,I6\nI0,I6,S4\nT1,Z3,I4\nL0,J3,L5,J8,T1\nL0,J3,L5,J8,T1,T6\nL0,J3,L5,J8,T1,T6,J2,L6,T0,T7\nL0,J3,L5,J8,T1,T6,J2,L6,T0,T7,Q4\nS0,S2,S4,S6\nS0,S2,S4,S5,Q8,Q8,Q8,Q8,T1,Q1,I0,Q4\nL0,J3,L5,J8,T1,T6,S2,Z5,T0,T7\nQ0,I2,I6,I0,I6,I6,Q2,Q4 ").split("\n")

  testString foreach {
    str ⇒ Tetris(str)
  }
}

