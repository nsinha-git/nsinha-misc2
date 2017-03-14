package com.nsinha.common

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/13/17.
  */
/*
For N+1 grid
Block00, Block0,1.... Block(o,n)
Block10, Block1,1.... Block(1,n)
--

Blockn0, Blockn,1.... Block(n,n)

for very Block GP is gridpoint:
   GPNW   GPNE
       Block
   GPSW    GPSE
 */

sealed trait BlockGridDir
object SE extends BlockGridDir
object SW extends BlockGridDir
object NE extends BlockGridDir
object NW extends BlockGridDir

sealed trait EdgeDir
object EastEdge extends EdgeDir
object WestEdge extends EdgeDir
object NorthEdge extends EdgeDir
object SouthEdge extends EdgeDir

class GridPoint(x : Int, y : Int) extends Coordinate(x, y)

class Block(x : Int, y : Int)(implicit val gridPoints : Map[Coordinate, GridPoint], implicit val edges : mutable.Map[(Coordinate, Coordinate), Edge])
  extends Coordinate(x, y) {
  val gpSE = getGridPointForDir(SE)
  val gpSW = getGridPointForDir(SW)
  val gpNE = getGridPointForDir(NE)
  val gpNW = getGridPointForDir(NW)

  val eastEdge = getEdge(EastEdge)
  val eastEdgeFlip = eastEdge.flip
  val westEdge = getEdge(WestEdge)
  val westEdgeFlip = eastEdge.flip
  val northEdge = getEdge(NorthEdge)
  val northEdgeFlip = eastEdge.flip
  val southEdge = getEdge(SouthEdge)
  val southEdgeFlip = eastEdge.flip

  private def getFromMapOfEdgesOrUpdateIt(lower : GridPoint, higher : GridPoint) : Edge = {
    if (edges.contains((lower, higher))) {
      edges((lower, higher))
    }
    else {
      val edge = new Edge(lower, higher)
      edges += (lower, higher) → edge
      edge
    }
  }

  def getEdge(dir : EdgeDir) : Edge = {
    dir match {
      case EastEdge  ⇒ getFromMapOfEdgesOrUpdateIt(gpNE, gpSE)
      case WestEdge  ⇒ getFromMapOfEdgesOrUpdateIt(gpNW, gpSW)
      case NorthEdge ⇒ getFromMapOfEdgesOrUpdateIt(gpNW, gpNE)
      case SouthEdge ⇒ getFromMapOfEdgesOrUpdateIt(gpSW, gpSE)
    }
  }

  def normalizeEdge(edge : Edge) : Option[Edge] = {
    edge match {
      case `eastEdge`      ⇒ Option(eastEdge)
      case `eastEdgeFlip`  ⇒ Option(eastEdge)
      case `westEdge`      ⇒ Option(westEdge)
      case `westEdgeFlip`  ⇒ Option(westEdge)
      case `northEdge`     ⇒ Option(northEdge)
      case `northEdgeFlip` ⇒ Option(northEdge)
      case `southEdge`     ⇒ Option(southEdge)
      case `southEdgeFlip` ⇒ Option(southEdge)
      case _               ⇒ None
    }

  }

  def getGridPointForDir(dir : BlockGridDir) : GridPoint = {
    dir match {
      case SE ⇒ gridPoints(Coordinate(x + 1, y + 1))
      case SW ⇒ gridPoints(Coordinate(x, y + 1))
      case NE ⇒ gridPoints(Coordinate(x + 1, y))
      case NW ⇒ gridPoints(Coordinate(x, y))
    }
  }

  val getAllGridPoints : List[GridPoint] = {
    List(gpSW, gpSE, gpNE, gpNW)
  }

  val getAllEdges : List[Edge] = {
    List(eastEdge, westEdge, northEdge, southEdge)
  }
}

class Edge(c1 : Coordinate, c2 : Coordinate) {
  def flip : Edge = {
    new Edge(c2, c1)
  }
}

trait GridTrait {
  def getAllGridPoints : List[GridPoint]
  def getAllBlocks : List[Block]
  def getAllSurroundingBlocksForGridPoint(gp : GridPoint) : Set[Block]
  def getAllSurroundingGridPointsForBlock(blk : Block) : Set[GridPoint]
  def getAllSurroundingBlocksForEdge(edge : Edge) : Set[Block]
  def getAllSurroundingEdgesForBlock(block : Block) : Set[Edge]
  def getAllBlocksForRow(r : Int) : Set[Block]
  def getAllBlocksForCol(r : Int) : Set[Block]
}

class Grid(rows : Int, cols : Int) extends GridTrait {
  var gridPointsToBlock = Map[GridPoint, mutable.HashSet[Block]]()
  var blockToGridPoints = Map[Block, mutable.HashSet[GridPoint]]()
  var edgesToBlock = Map[Edge, mutable.HashSet[Block]] ()
  var blockToEdges = Map[Block, mutable.HashSet[Edge]] ()
  var rowToBlocks = Map[Int, mutable.HashSet[Block]]()
  var colToBlocks = Map[Int, mutable.HashSet[Block]]()

  override def getAllGridPoints : List[GridPoint] = gridPoints.values.toList
  override def getAllBlocks : List[Block] = blocks.values.toList
  override def getAllSurroundingBlocksForGridPoint(gp : GridPoint) : Set[Block] = gridPointsToBlock(gp).toSet
  override def getAllSurroundingGridPointsForBlock(blk : Block) : Set[GridPoint] = blockToGridPoints(blk).toSet
  override def getAllSurroundingBlocksForEdge(edge : Edge) : Set[Block] = edgesToBlock(edge).toSet
  override def getAllSurroundingEdgesForBlock(block : Block) : Set[Edge] = blockToEdges(block).toSet
  override def getAllBlocksForRow(r : Int) : Set[Block] = rowToBlocks(r).toSet
  override def getAllBlocksForCol(r : Int) : Set[Block] = colToBlocks(r).toSet

  implicit val gridPoints : Map[Coordinate, GridPoint] = {
    {
      for {
        x ← Range(0, rows + 1).toList
        y ← Range(0, cols + 1).toList
      } yield {
        Coordinate(x, y) → new GridPoint(x, y)
      }
    } toMap
  }

  implicit val edges = mutable.Map[(Coordinate, Coordinate), Edge] ()

  implicit val blocks : Map[Coordinate, Block] = {
    {
      for {
        x ← Range(0, rows).toList
        y ← Range(0, cols).toList
      } yield {
        val blk = new Block(x, y)
        updateGridPointsToBlock(blk, blk.getAllGridPoints)
        updateBlockToGridPoints(blk, blk.getAllGridPoints)
        updateBlockToEdges(blk, blk.getAllEdges)
        updateEdgesToBlock(blk, blk.getAllEdges)
        updateRowToBlock(blk)
        updateColToBlock(blk)
        Coordinate(x, y) → blk
      }
    }.toMap
  }

  def updateGridPointsToBlock(b : Block, gps : List[GridPoint]) = {
    gps foreach {
      gp ⇒
        if (!gridPointsToBlock.contains(gp)) {
          gridPointsToBlock += (gp → mutable.HashSet[Block]())
        }
        gridPointsToBlock(gp) += (b)
    }
  }

  def updateBlockToGridPoints(b : Block, gps : List[GridPoint]) = {
    if (!blockToGridPoints.contains(b)) {
      blockToGridPoints += (b → mutable.HashSet[GridPoint]())
    }
    gps foreach {
      gp ⇒
        blockToGridPoints(b) += (gp)
    }
  }

  def updateEdgesToBlock(b : Block, edges : List[Edge]) = {
    edges foreach {
      edge ⇒
        if (!edgesToBlock.contains(edge)) {
          edgesToBlock += (edge → mutable.HashSet[Block]())
        }
        edgesToBlock(edge) += (b)
    }
  }

  def updateBlockToEdges(b : Block, edges : List[Edge]) = {
    if (!blockToEdges.contains(b)) {
      blockToEdges += (b → mutable.HashSet[Edge]())
    }
    edges foreach {
      edge ⇒
        blockToEdges(b) += (edge)
    }
  }

  def updateRowToBlock(b : Block) : Unit = {
    val row : Int = b.x
    if (!rowToBlocks.contains(row)) {
      rowToBlocks += (row → mutable.HashSet[Block]())
    }
    rowToBlocks(row) += b
  }

  def updateColToBlock(b : Block) : Unit = {
    val col : Int = b.x
    if (!colToBlocks.contains(col)) {
      colToBlocks += (col → mutable.HashSet[Block]())
    }
    colToBlocks(col) += b
  }
}

class Testing extends FunSuite {

  test ("a") {
    val chess3x3 = new Grid(3, 3)
    print(chess3x3)
  }
}

