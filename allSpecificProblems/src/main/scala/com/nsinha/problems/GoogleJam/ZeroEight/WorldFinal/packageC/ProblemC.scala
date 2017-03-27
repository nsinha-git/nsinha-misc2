package com.nsinha.problems.GoogleJam.ZeroEight.WorldFinal.packageC

import com.nsinha.common.{Block, Coordinate, Grid}
import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/17/17.
  */
object ProblemC {

  def getTheSurroundingBlocksIncludeingSWNWNESE(g : Grid, b : Block) : Set[Block] = {
    val allGps = g.getAllSurroundingGridPointsForBlock(b)
    val surroundinBlocks = allGps.foldLeft(Set[Block]()) { (Z, el) ⇒
      val x = g.getAllSurroundingBlocksForGridPoint(el)
      Z ++ x
    }
    surroundinBlocks
  }

  def getTheGridWithNumOfNeighbors(g : Grid) : Map[Block, Int] = {
    g.blocks map {
      x ⇒
        val surroundinBlocks = getTheSurroundingBlocksIncludeingSWNWNESE(g, x._2)
        (x._2, surroundinBlocks.size)
    }
  }

  def getSetOfFreeBlocks(s : String) : Set[Int] = {
    val allRows = s.split("\n")
    allRows.foldLeft(0, Set[Int]()) {
      (Z, el) ⇒
        val curSet = el.foldLeft(0, Set[Int]()) { (ZZ, ell) ⇒
          if (ell == '0') {
            (ZZ._1 + 1, ZZ._2 + ZZ._1)
          }
          else {
            (ZZ._1 + 1, ZZ._2)
          }
        }._2 map { x ⇒ x + Z._1 }
        (Z._1 + el.size, Z._2 ++ curSet)
    }._2
  }

}

case class ProblemCSolve(rows : Int, cols : Int, inputs : String) {

  val grid = new Grid(rows, cols)

  val initialgridValues = {
    val res = mutable.Map[Block, Int]()
    var blkCnt = 0
    inputs.split("\n") foreach {
      x ⇒
        x foreach { char ⇒
          if (char != ' ') {
            res += grid.getTheBlockNumbered(blkCnt) → (char.toInt - 48)
            blkCnt = blkCnt + 1
          }
        }
    }
    res.toMap
  }

  //val prob = ProblemCBruteForceInt(grid, initialgridValues)

  //prob.zeroMaps

  val prob1 = ProblemCBruteForceFillNonFill(grid, initialgridValues)
}

case class ProblemCBruteForceInt(grid : Grid, gridValues : Map[Block, Int]) {

  val dpTable : mutable.HashMap[Int, mutable.HashMap[String, Map[Block, Int]]] = mutable.HashMap[Int, mutable.HashMap[String, Map[Block, Int]]]()
  val zeroMaps : mutable.Set[String] = mutable.Set[String]()
  val canTry : mutable.Set[Block] = mutable.Set[Block]()

  val allZeroStringForMines : String = {
    val aLine : String = { Range(0, grid.cols) map { x ⇒ '0' } }.mkString("")
    val res = Range(0, grid.rows) map { x ⇒ aLine }
    res.mkString("\n")
  }

  val sumOfAllGrids = gridValues.foldLeft(0) { (Z, el) ⇒ Z + el._2 }

  //we are going to take mines from aboutRange before it finishes. If no success till then we are finished with failure.
  //else if we get success may be with few balls remaining we are still good.
  val aboutRange : Int = ((sumOfAllGrids + grid.rows * 2 + grid.cols * 2 + 4) * (1.2 / 9)).toInt;

  val blockToNumOfNeighborsMap : Map[Block, Int] = ProblemC.getTheGridWithNumOfNeighbors(grid)

  tryFullSearch

  def tryFullSearch = {
    initDpTable()
    var stage = 2
    var cond = true
    while (cond == true) {
      cond = fillDpTable(stage)
      stage = stage + 1
    }
    if (zeroMaps.nonEmpty) {
      println(s"${zeroMaps.size} sols found.")
      zeroMaps foreach (x ⇒ println(x+"\n"))

    }
    else {
      println("No sol found")
    }
  }

  def initDpTable() = {
    dpTable += 1 → mutable.HashMap[String, Map[Block, Int]]()
    for (blk ← grid.blocks) {
      val res = evaluateByFillingBlock(allZeroStringForMines, gridValues, blk._2)
      if (!res._3) {
        dpTable(1) += res._1 → res._2
        canTry += blk._2
      }
    }
  }

  def fillDpTable(stage : Int) : Boolean = {
    //stage n+1 depends on stage n
    dpTable += stage → mutable.HashMap[String, Map[Block, Int]]()
    dpTable filter (x ⇒ x._1 == (stage - 1)) map {
      configTable ⇒
        val innerTable = configTable._2
        innerTable filter { x ⇒ x._2.nonEmpty } map { x ⇒
          val setOfFreeBlocks : Set[Int] = ProblemC.getSetOfFreeBlocks(x._1)

          setOfFreeBlocks foreach { freeBlock ⇒
            if (canTry.contains(grid.getTheBlockNumbered(freeBlock))) {
              val res = evaluateByFillingBlock(x._1, x._2, freeBlock)
              if (!res._3) dpTable(stage) += (res._1 → res._2)
            }
          }
        }
    }
    println(s"stage = $stage sizeOfDp = ${dpTable(stage).size}")
    dpTable(stage).nonEmpty
  }

  def evaluateByFillingBlock(s : String, mp : Map[Block, Int], n : Int) : (String, Map[Block, Int], Boolean) = {
    val blk = grid.getTheBlockNumbered(n)
    evaluateByFillingBlock(s, mp, blk)
  }

  def evaluateByFillingBlock(s : String, mp : Map[Block, Int], blk : Block) : (String, Map[Block, Int], Boolean) = {
    assert(blk != null)
    val newS = changeBlockStringAtRowCol(blk.x, blk.y, s)
    val allBlks = ProblemC.getTheSurroundingBlocksIncludeingSWNWNESE(grid, blk)
    var fail = false
    //alter the map of all these
    val resMap = mp map { x ⇒
      if (allBlks.contains(x._1)) {
        if (x._2 - 1 < 0) fail = true
        x._1 → (x._2 - 1)
      }
      else
        x
    }
    var mpSum = 0
    resMap foreach (x ⇒ mpSum = x._2 + mpSum)
    if (mpSum == 0 && !fail) {
      zeroMaps += newS
      //println(s)
      //println(mp)
      //println(s"blk :${blk.x} ${blk.y}")
      //println("New Orientation")
      //println(newS)
      //println()
      //println(resMap)
      //println()

    }

    (newS, resMap, fail)
  }

  def changeBlockStringAtRowCol(row : Int, col : Int, s : String) : String = {
    val sArr = s.split("\n")
    val toChange = sArr(row)
    val changed = toChange.foldLeft(List[Char](), 0) { (Z, el) ⇒
      val char = if (Z._2 == col) {
        'x'
      }
      else {
        el
      }
      (Z._1 :+ char, Z._2 + 1)
    }._1.mkString("")
    sArr(row) = changed
    sArr.mkString("\n")
  }

}

sealed trait FillStatus
object Filled extends FillStatus
object Spaced extends FillStatus
object Undecided extends FillStatus

case class ProblemCBruteForceFillNonFill(grid : Grid, gridValues : Map[Block, Int]) {
  val dpTable = mutable.HashMap[DpKey, mutable.HashMap[DpKey, Map[Block, FillStatus]]] ()
  val zeroMaps : mutable.Set[Map[Block, FillStatus]] = mutable.Set[Map[Block, FillStatus]]()
  val canTry : mutable.Set[Block] = mutable.Set[Block]()

  val allZeroStringForMines : String = {
    val aLine : String = { Range(0, grid.cols) map { x ⇒ '0' } }.mkString("")
    val res = Range(0, grid.rows) map { x ⇒ aLine }
    res.mkString("\n")
  }

  val sumOfAllGrids = gridValues.foldLeft(0) { (Z, el) ⇒ Z + el._2 }

  val blockToNumOfNeighborsMap : Map[Block, Int] = ProblemC.getTheGridWithNumOfNeighbors(grid)

  tryFullSearch

  def tryFullSearch = {

    val res = makeTopDownDpTable(gridValues, gridValues map { x ⇒ x._1 → Undecided })

    if (zeroMaps.nonEmpty) {
      println(s"${zeroMaps.size} sols found.")
      zeroMaps foreach (x ⇒ println(convertFillMapToStringForDpKey(x)))
    }
    else {
      println("No sol found")
    }
  }

  case class BlkProb(mines : Int, neighbors : Int, prob : Double)

  def findProbBlkNo(blkNo : Int, mp : Map[Block, Int], mpStatus : Map[Block, FillStatus]) : BlkProb = {
    findProbBlk(grid.getTheBlockNumbered(blkNo), mp, mpStatus)
  }

  def findProbBlk(blk : Block, mp : Map[Block, Int], mpStatus : Map[Block, FillStatus]) : BlkProb = {
    val surroundingBlocks = ProblemC.getTheSurroundingBlocksIncludeingSWNWNESE(grid, blk)
    var totalUndecidedNeighbors = 0
    surroundingBlocks foreach { thisBlk ⇒
      if (mpStatus(thisBlk) == Undecided) totalUndecidedNeighbors = totalUndecidedNeighbors + 1
    }
    BlkProb(mp(blk), totalUndecidedNeighbors, 1.0 * mp(blk) / totalUndecidedNeighbors)
  }
  case class DpKey(s : String) extends Ordered[DpKey] {
    val sNormalized = s.replace("U", "S")
    override def compare(that : DpKey) : Int = { sNormalized.compare(that.sNormalized) }
    override def toString : String = sNormalized
  }

  def convertFillMapToStringForDpKey(mp : Map[Block, FillStatus]) : DpKey = {
    val strBuf = new StringBuilder
    for (row ← Range(0, grid.rows)) {
      for (col ← Range(0, grid.cols)) {
        val blk = grid.blocks(Coordinate(row, col))
        if (mp(blk) == Spaced) { strBuf += 'S' }
        if (mp(blk) == Filled) { strBuf += 'F' }
        if (mp(blk) == Undecided) { strBuf += 'U' }
      }
      strBuf += '\n'
    }
    DpKey(strBuf.toString())
  }

  def setAllProbNMinusOneFills(mp : Map[Block, Boolean], mapMines : Map[Block, Int], mapFills : Map[Block, FillStatus]) : (Map[Block, Int], Map[Block, FillStatus]) = {
    val res = { mp filter (p ⇒ p._2 == false) }.foldLeft(mapFills, mapMines) {
      (Z, blkStatus) ⇒
        val blk = blkStatus._1
        val newMapMines = reCalculateMapToCurBlockToMines(blk, Filled, Z._2)

        val newMapFills = Z._1.-(blk).+(blk → Filled)
        (newMapFills, newMapMines)
    }
    (res._2, res._1)
  }

  def reCalculateMapToCurBlockToMines(blk : Block, fillStatus : FillStatus, mapBlockToMines : Map[Block, Int]) : Map[Block, Int] = {
    if (fillStatus == Filled) {
      ProblemC.getTheSurroundingBlocksIncludeingSWNWNESE(grid, blk).foldLeft(mapBlockToMines) { (ZZ, ell) ⇒
        ZZ + (ell → (ZZ(ell) - 1))
      }
    }
    else {
      mapBlockToMines
    }
  }

  def findOneOffBlocks(sortedAllCandidatesToProb : List[(Block, BlkProb)]) : Set[Block] = {
    val res = mutable.Set[Block]()
    sortedAllCandidatesToProb foreach {
      x ⇒
        val diff = x._2.neighbors - x._2.mines
        if (diff == 1) {
          res += x._1
        }
    }
    res.toSet
  }

  def setZeroSpaces(mpCurBlockToMines : Map[Block, Int], mapStatus : Map[Block, FillStatus], sortedAllCandidatesToProb : List[(Block, BlkProb)]) : (Map[Block, Int], Map[Block, FillStatus]) = {
    val newMapStatus = mutable.HashMap[Block, FillStatus]() ++ mapStatus
    var revSorted = sortedAllCandidatesToProb.reverse
    var cond = true
    while (cond && revSorted.nonEmpty) {
      val head = revSorted.head
      val blk = head._1
      revSorted = revSorted.drop(1)
      if (head._2.mines == 0) {
        val allNeighbors = ProblemC.getTheSurroundingBlocksIncludeingSWNWNESE(grid, blk)
        //set allNeighbors that are undecided to spaced
        allNeighbors foreach { curBlk ⇒
          if (newMapStatus(curBlk) == Undecided) {
            newMapStatus(curBlk) = Spaced
          }
        }
      }
      else {
        cond = false
      }
    }
    (mpCurBlockToMines, newMapStatus.toMap)

  }

  def setTopProbOneFills(mpCurBlockToMines : Map[Block, Int], mapStatus : Map[Block, FillStatus], sortedAllCandidatesToProb : List[(Block, BlkProb)]) : (Map[Block, Int], Map[Block, FillStatus], Boolean) = {
    val res = { sortedAllCandidatesToProb filter (x ⇒ x._2.mines == x._2.neighbors) }.sortBy(x ⇒ -x._2.mines)
    if (res.nonEmpty) {
      val blk = res.head._1
      val allNeighbors : Set[Block] = ProblemC.getTheSurroundingBlocksIncludeingSWNWNESE(grid, blk)
      //set mine at neighbor and update the map, one neighbor at a time.
      val newMapStatus = allNeighbors.filter(x ⇒ mapStatus(x) == Undecided).foldLeft(mapStatus) { (ZZ, ell) ⇒
        ZZ.+(ell → Filled)
      }
      val newMapBlockToMines = allNeighbors.foldLeft(mpCurBlockToMines){ (ZZ, ell) ⇒
        if (mapStatus(ell) == Undecided) {
          val x = reCalculateMapToCurBlockToMines(ell, Filled, ZZ)
          x
        }
        else {
          ZZ
        }
      }
      (newMapBlockToMines, newMapStatus, false)
    }
    else {
      (mpCurBlockToMines, mapStatus, true)
    }
  }

  def createSortedListOfProbableMines(mapMines : Map[Block, Int], mapStatus : Map[Block, FillStatus]) : List[(Block, BlkProb)] = {
    val allCandidateBlks = {
      mapStatus //filter (x => x._2 == Undecided)
    }.toList map (_._1)

    val allCandidatesToProb = allCandidateBlks map { blk ⇒
      (blk, findProbBlk(blk, mapMines, mapStatus))
    } toMap

    allCandidatesToProb.toList.sortBy(x ⇒ -x._2.prob)
  }

  def moreMinesThanNeighbors(sortedOnProb : List[(Block, BlkProb)]) : Boolean = {
    if (sortedOnProb.nonEmpty) {
      val probHead = sortedOnProb.head._2
      probHead.mines > probHead.neighbors
    }
    else {
      false
    }
  }

  def printBoard(mines : Map[Block, Int], fillMap : Map[Block, FillStatus]) : String = {
    var total = 0
    var filled = 0
    var spaced = 0
    var undecided = 0
    var strBuf = ""
    for (row ← Range(0, grid.rows)) {
      for (col ← Range(0, grid.cols)) {
        total = total + 1
        val blk = grid.blocks(Coordinate(row, col))
        val numMines = mines(blk)
        val fillStatus = fillMap(blk)

        fillStatus match {
          case Filled ⇒
            strBuf = strBuf + s"(F, ${numMines}), "
            filled = filled + 1
          case Spaced ⇒
            strBuf = strBuf + s"(S, ${numMines}), "
            spaced = spaced + 1
          case Undecided ⇒
            strBuf = strBuf + s"(U, ${numMines}), "
            undecided = undecided + 1
        }

      }
      strBuf = strBuf + '\n'
    }
    strBuf + s"$total $filled $spaced $undecided\n"
  }

  def createMinesString(minesMap : Map[Block, Int]) : String = {
    val strBuf = new StringBuilder
    for (row ← Range(0, grid.rows)) {
      for (col ← Range(0, grid.cols)) {
        val x = minesMap(grid.blocks(Coordinate(row, col)))
        strBuf += x.toString.charAt(0)
      }
      strBuf += '\n'
    }
    strBuf.toString()
  }

  def makeTopDownDpTable(mpCurBlockToMines : Map[Block, Int], mapStatus : Map[Block, FillStatus]) : mutable.Map[DpKey, Map[Block, FillStatus]] = {
    //if this is first call we have empty mapOStatus
    //we need to find probability of attrituable blocks for each candidate

    //base case
    var numMinesAttribution = mpCurBlockToMines.foldLeft(0, true) { (Z, el) ⇒
      val cond = if (el._2 < 0) false else true
      val newMineCnt = if (el._2 > 0) el._2 + Z._1 else Z._1
      (newMineCnt, Z._2 & cond)
    }

    val dpKey = convertFillMapToStringForDpKey(mapStatus)
    val minesKey = createMinesString(mpCurBlockToMines)
    println(numMinesAttribution)
    println(printBoard(mpCurBlockToMines, mapStatus))

    if (dpTable.contains(dpKey)) {
      return dpTable(dpKey)
    }

    if (numMinesAttribution._2 && numMinesAttribution._1 == 0) {
      if (!dpTable.contains(dpKey)) {
        dpTable(dpKey) = mutable.HashMap[DpKey, Map[Block, FillStatus]]()
        dpTable(dpKey) += (dpKey → mapStatus)
        zeroMaps += mapStatus
        return (dpTable(dpKey))
      }
    }

    if (numMinesAttribution._2 == false) {
      //overfilling case . At this point we could also say that any U or S if changed to F will be equally a failure as we cant bring in any more F
      //we need a 2D contained fn that can check if a key in map is contained inside the testing key.
      //
      if (!dpTable.contains(dpKey)) {
        dpTable(dpKey) = mutable.HashMap[DpKey, Map[Block, FillStatus]]()
        return (dpTable(dpKey))
      }
      else {
        return (dpTable(dpKey))
      }
    }

    val sortedAllCandidatesToProb = createSortedListOfProbableMines(mpCurBlockToMines, mapStatus)

    if (moreMinesThanNeighbors(sortedAllCandidatesToProb)) {
      val prevRes = mutable.HashMap[DpKey, Map[Block, FillStatus]]()
      dpTable += (dpKey → prevRes)
      return prevRes
    }
    //fully do the prob Ones

    val (newMpCurBlockToMines, newMapStatus, newSortedAllCandidatesToProb) = {
      var allOneDone = false
      var newMpCurBlockToMinesTemp = mpCurBlockToMines
      var newMapStatusTemp = mapStatus
      var newSortedAllCandidatesToProbTemp : List[(Block, BlkProb)] = createSortedListOfProbableMines(newMpCurBlockToMinesTemp, newMapStatusTemp)
      while (!allOneDone) {
        val res = setTopProbOneFills(newMpCurBlockToMinesTemp, newMapStatusTemp, newSortedAllCandidatesToProbTemp filter (x ⇒ newMapStatusTemp(x._1) == Undecided))
        newMpCurBlockToMinesTemp = res._1
        newMapStatusTemp = res._2
        allOneDone = res._3
        newSortedAllCandidatesToProbTemp = createSortedListOfProbableMines(newMpCurBlockToMinesTemp, newMapStatusTemp)

        if (moreMinesThanNeighbors(newSortedAllCandidatesToProbTemp)) {
          val prevRes = mutable.HashMap[DpKey, Map[Block, FillStatus]]()
          dpTable += (dpKey → prevRes)
          return prevRes
        }
      }
      (newMpCurBlockToMinesTemp, newMapStatusTemp, newSortedAllCandidatesToProbTemp)
    }

    //set all zero neighbors to spaced
    val (newMpCurBlockToMinesZeroRemoval, newMapStatusAfterZeroRemoval) = setZeroSpaces(newMpCurBlockToMines, newMapStatus, newSortedAllCandidatesToProb)

    val newDpKeyOnProbOne = convertFillMapToStringForDpKey(newMapStatusAfterZeroRemoval)
    val newMinesKey = createMinesString(newMpCurBlockToMinesZeroRemoval)
    println()
    println(printBoard(newMpCurBlockToMinesZeroRemoval, newMapStatusAfterZeroRemoval))

    var numMinesAttributionCur = newMpCurBlockToMinesZeroRemoval.foldLeft(0, true) { (Z, el) ⇒
      val cond = if (el._2 < 0) false else true
      val newMineCnt = if (el._2 > 0) el._2 + Z._1 else Z._1
      (newMineCnt, Z._2 & cond)
    }

    if (numMinesAttributionCur._2 == false) {
      //overfilling case . At this point we could also say that any U or S if changed to F will be equally a failure as we cant bring in any more F
      //we need a 2D contained fn that can check if a key in map is contained inside the testing key.
      //
      if (!dpTable.contains(newDpKeyOnProbOne)) {
        dpTable(newDpKeyOnProbOne) = mutable.HashMap[DpKey, Map[Block, FillStatus]]()
        return (dpTable(newDpKeyOnProbOne))
      }
      else {
        return (dpTable(newDpKeyOnProbOne))
      }
    }

    //at this point we should just do a full search on rest of blocks by setting them one by one
    val res2 = newSortedAllCandidatesToProb filter (x ⇒ newMapStatusAfterZeroRemoval(x._1) == Undecided) map {
      x ⇒
        val blkToTryFill = x._1
        val nextCurBlockToMines = reCalculateMapToCurBlockToMines(blkToTryFill, Filled, newMpCurBlockToMinesZeroRemoval)
        val nextMapStatus = newMapStatusAfterZeroRemoval.-(blkToTryFill).+(blkToTryFill → Filled)
        makeTopDownDpTable(nextCurBlockToMines, nextMapStatus)
    }

    val res2Combined = if (res2.nonEmpty) {
      val x = res2.reduce((x, y) ⇒ x.++(y))
      x
    }
    else mutable.Map[DpKey, Map[Block, FillStatus]]()
    /*val offOneCombined = doesOffBranchWork.toList.foldLeft(res2Combined) {(Z, el) =>
      Z ++ el._2
    }*/

    //store it in dp
    if (dpTable.contains(dpKey)) {
      val prevRes = dpTable(dpKey)
      res2Combined foreach (x ⇒ prevRes += x)
    }
    else {
      val prevRes = mutable.HashMap[DpKey, Map[Block, FillStatus]]()
      res2Combined foreach (x ⇒ prevRes += x)
      dpTable += (dpKey → prevRes)
    }

    if (newDpKeyOnProbOne != dpKey) {
      if (dpTable.contains(newDpKeyOnProbOne)) {
        val prevRes = dpTable(newDpKeyOnProbOne)
        res2Combined foreach (x ⇒ prevRes += x)
      }
      else {
        val prevRes = mutable.HashMap[DpKey, Map[Block, FillStatus]]()
        res2Combined foreach (x ⇒ prevRes += x)
        dpTable += (newDpKeyOnProbOne → prevRes)
      }
    }
    res2Combined
  }

  def changeBlockStringAtRowCol(row : Int, col : Int, s : String) : String = {
    val sArr = s.split("\n")
    val toChange = sArr(row)
    val changed = toChange.foldLeft(List[Char](), 0) { (Z, el) ⇒
      val char = if (Z._2 == col) {
        'x'
      }
      else {
        el
      }
      (Z._1 :+ char, Z._2 + 1)
    }._1.mkString("")
    sArr(row) = changed
    sArr.mkString("\n")
  }
}

class TestingC extends FunSuite {

  test("a") {
    ProblemCSolve(5, 4,
      """4 6 5 3
        |6 9 8 5
        |6 8 8 5
        |4 6 7 5
        |2 3 4 3""".stripMargin)

  }

  test("b") {
    ProblemCSolve(5, 5,
      """2 3 5 4 3
        |4 6 8 7 5
        |5 8 8 7 4
        |6 9 8 8 5
        |4 6 5 5 3""".stripMargin)

  }

  test("c") {
    ProblemCSolve(45, 37,
      """3 4 4 5 5 5 4 4 4 4 4 2 3 3 4 4 4 4 3 3 4 5 5 5 4 4 4 5 4 4 2 4 3 4 4 4 3
        |4 6 7 8 8 8 7 7 7 6 6 3 5 4 5 4 4 5 5 6 7 8 7 7 5 6 6 7 6 5 4 6 6 6 6 6 5
        |4 7 7 8 7 8 6 7 7 7 6 4 5 5 6 5 4 4 5 7 7 7 5 6 5 6 6 7 6 5 4 5 5 5 6 7 6
        |3 6 7 8 8 9 8 7 7 6 7 6 6 4 3 3 4 5 7 8 8 7 6 7 6 7 7 8 7 5 5 4 5 4 6 7 6
        |3 6 6 8 8 8 6 5 6 6 7 6 6 5 5 5 6 5 7 6 7 6 7 8 8 7 7 8 8 7 5 4 3 4 6 8 6
        |2 5 7 8 7 5 4 4 6 7 8 7 7 5 4 3 4 5 7 7 8 8 9 9 8 6 5 6 8 8 7 5 4 4 5 6 4
        |2 4 5 5 4 3 2 4 6 8 8 7 7 5 4 3 4 4 6 5 7 6 8 8 8 6 4 5 7 8 6 5 4 6 6 7 4
        |2 4 6 5 4 4 3 4 5 8 8 7 5 4 3 4 4 5 6 7 7 6 7 8 7 5 2 3 5 7 6 5 5 6 6 6 4
        |2 4 5 6 6 7 6 6 6 7 7 6 5 5 4 6 5 5 5 5 6 5 7 8 8 7 5 5 5 6 4 5 6 8 8 8 6
        |4 7 8 9 9 8 7 6 7 6 5 3 4 6 7 7 4 3 3 5 6 7 8 9 8 7 6 6 6 6 4 5 6 8 8 8 6
        |4 7 8 9 9 8 8 8 8 6 5 5 7 7 7 6 5 4 4 5 6 6 7 8 9 9 9 8 7 6 5 6 6 7 6 6 4
        |5 7 8 8 8 7 7 8 7 5 4 5 7 7 8 7 6 4 4 6 7 7 6 7 8 8 8 7 7 6 6 7 7 7 6 5 3
        |5 7 8 8 7 7 6 8 5 4 4 7 8 7 6 7 7 7 6 7 7 6 4 5 7 8 8 6 6 6 7 7 6 5 4 2 1
        |5 6 7 6 5 4 4 6 5 4 4 6 7 8 6 7 6 7 6 6 6 6 5 5 5 6 6 5 4 5 6 7 7 6 6 4 3
        |4 6 8 7 5 3 2 4 4 4 4 5 6 7 6 7 7 9 8 7 6 6 5 5 5 7 6 4 3 5 7 6 6 5 6 5 4
        |2 4 7 7 6 4 4 4 5 5 6 5 6 7 7 6 6 8 9 8 6 6 6 6 4 4 4 4 4 5 7 7 8 6 7 7 6
        |3 6 7 7 6 6 6 6 6 6 6 4 4 4 6 6 7 8 9 9 8 8 7 7 6 6 5 5 5 6 6 5 5 5 6 7 5
        |4 6 6 6 6 7 7 7 7 8 8 6 5 4 5 4 6 7 8 8 7 8 7 7 6 6 6 6 6 6 6 6 6 6 6 6 4
        |6 7 6 5 6 7 7 8 8 9 7 6 5 4 4 3 6 7 8 8 7 8 6 7 6 7 6 6 5 6 6 7 6 6 6 6 4
        |6 6 6 6 7 7 5 6 5 6 5 6 6 5 4 3 5 5 6 7 7 8 6 7 6 6 6 6 7 6 7 7 7 6 6 5 3
        |5 6 7 7 8 8 6 6 4 5 4 6 6 5 3 3 5 5 6 7 8 8 6 7 5 4 3 5 6 6 5 5 5 5 6 6 4
        |5 7 8 8 8 8 6 6 4 4 4 6 7 6 4 4 5 4 5 6 9 9 8 7 6 5 4 4 5 5 5 3 3 4 6 7 4
        |4 6 7 6 6 6 6 7 7 7 7 8 9 8 6 5 6 5 6 6 9 8 6 4 4 4 4 3 4 4 5 3 4 5 8 8 5
        |4 6 6 5 4 3 5 7 9 7 6 5 6 6 6 6 7 5 6 5 8 6 6 4 6 6 7 4 5 4 7 4 5 5 8 7 4
        |3 4 5 3 3 1 4 6 8 7 6 6 5 5 6 8 9 6 6 5 7 5 5 5 7 7 8 6 7 6 7 5 5 6 7 5 2
        |3 4 5 4 5 2 5 6 7 6 4 4 3 4 5 7 7 6 5 5 5 5 6 8 8 8 8 8 8 8 8 7 6 7 7 6 3
        |3 4 6 5 7 5 7 7 6 5 3 5 5 7 6 7 6 7 5 6 5 6 6 7 7 6 7 7 8 8 8 7 6 6 6 6 4
        |4 6 7 6 7 5 7 7 7 6 4 4 4 6 6 7 6 8 6 7 6 7 7 7 7 5 6 6 7 7 7 7 7 7 7 7 5
        |3 6 8 8 8 6 6 5 5 5 5 6 6 6 6 7 7 7 6 7 8 7 6 5 6 5 6 5 6 6 7 7 7 7 6 5 3
        |4 6 6 5 6 5 6 4 5 6 7 7 6 5 5 5 5 5 6 7 8 7 6 5 5 5 6 6 6 6 5 6 6 8 7 6 3
        |4 6 6 6 7 6 5 3 4 6 6 6 6 5 4 2 3 4 6 7 8 6 5 4 5 5 6 6 6 5 5 6 7 7 6 5 3
        |6 8 7 5 5 5 6 6 6 7 6 5 5 4 3 1 3 6 7 7 6 6 6 6 7 5 6 5 6 4 4 5 7 7 6 6 4
        |5 7 8 7 7 6 5 5 4 6 5 5 5 5 4 3 5 8 8 7 5 5 5 7 7 5 4 4 6 4 5 6 8 7 5 5 3
        |3 4 5 4 5 6 6 5 3 5 6 6 6 5 5 5 6 8 7 7 5 6 6 8 7 6 5 5 7 6 7 7 8 7 6 5 3
        |3 3 4 4 7 7 6 4 3 5 6 7 6 6 6 6 6 7 7 7 5 6 6 7 5 4 4 6 7 7 7 8 7 6 6 6 4
        |4 4 4 4 7 7 6 5 4 6 7 8 6 5 5 5 5 5 6 6 5 5 6 6 6 5 7 8 8 8 7 8 6 6 5 5 3
        |5 6 6 7 9 8 7 7 6 6 5 7 5 6 5 5 4 3 5 5 5 5 5 5 4 4 6 8 7 7 6 8 5 6 4 6 3
        |3 4 5 7 9 9 8 7 5 5 5 7 5 6 5 5 3 2 5 6 7 6 5 4 3 5 7 9 8 8 7 7 4 4 3 4 2
        |3 4 4 6 8 9 8 7 5 4 3 5 4 5 4 4 3 3 5 7 8 9 7 6 4 6 6 8 7 8 8 8 5 3 3 5 4
        |4 5 4 5 6 7 6 5 4 3 4 6 6 5 4 4 5 5 6 6 6 7 7 7 5 6 5 6 6 8 9 8 6 3 3 3 3
        |5 6 5 5 6 7 6 5 5 4 4 4 5 4 4 4 7 7 6 4 4 6 7 8 7 7 4 3 4 7 9 9 8 6 5 5 4
        |4 5 4 5 6 6 5 3 5 5 7 6 6 4 5 6 9 8 6 4 4 6 7 7 6 6 5 4 5 7 9 9 8 7 6 6 4
        |2 3 4 6 8 7 6 4 6 7 8 7 6 5 5 6 8 9 7 6 6 7 6 5 5 6 6 6 6 6 6 7 7 8 8 9 6
        |3 5 5 7 8 7 5 3 5 6 8 8 8 7 6 7 7 7 6 6 6 5 5 4 5 6 8 8 7 5 5 5 5 6 7 9 6
        |2 4 5 6 6 5 4 3 4 4 5 5 6 6 5 5 4 4 4 4 4 2 2 2 3 4 5 5 4 2 2 2 3 4 5 6 4""".stripMargin)

  }

}

