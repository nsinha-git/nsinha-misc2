package com.nsinha.problems.GoogleJam.Ten.WorldFInal.ProblemB

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 5/26/17.
  *
  *
  */

case class Node(name : Int, nbrs : mutable.Set[Int])

class PrefixPath {
  var buffer = {
    val arr = new Array[Int](1000)
    for (i ← Range(0, 1000)) {
      arr(i) = 0
    }
    arr
  }

  var offset = 0
  val hashMap = mutable.HashMap[Int, Int]()

  def addMutable(i : Int) = {
    buffer(offset) = i
    hashMap += i → offset
    offset = offset + 1
  }
  def addNonMutable(i : Int) : PrefixPath = {
    val n = new PrefixPath
    for (j ← Range(0, offset)) {
      n.addMutable(buffer(j))
    }
    n.addMutable(i)
    n
  }

  def search(i : Int) : Option[Int] = { hashMap.get(i) }

  def subtract(that : PrefixPath) : Int = {
    //we assume that is enshrined inside this
    this.offset - that.offset
  }

  def contains(that : PrefixPath) : Boolean = {
    if (that.offset <= this.offset) {
      val allEqual = for (i ← Range(0, that.offset)) yield {
        buffer(i) == that.buffer
      }
      allEqual.forall(_ == true)
    }
    else {
      false
    }
  }

}

sealed trait DfsVisitState
object Unvisited extends DfsVisitState
object Visited extends DfsVisitState
object StillVisiting extends DfsVisitState

class Graph(sz : Int) {
  var mapOfNodes = {
    val mp = mutable.Map[Int, Node]()

    for (i ← Range(1, sz + 1)) {
      mp += (i → Node(i, mutable.Set[Int]()))
    }
    mp
  }
  val mapOfNodeState = mutable.HashMap[Int, DfsVisitState]()
  val mapOfNodePrefixes = mutable.HashMap[Int, PrefixPath]()

  var maxTour = 0

  def addNode(s : Int, nbrs : List[Int]) = {
    val nodeSrc = mapOfNodes(s)
    mapOfNodeState += s → Unvisited
    //we need to update the nbrs too
    nbrs foreach { nbr ⇒
      val node = mapOfNodes(nbr)
      node.nbrs += s
      nodeSrc.nbrs += nbr
    }
  }

  def findMaxTour : Int = {
    dfsOnNode(mapOfNodes(1), new PrefixPath)
    maxTour
  }

  def dfsOnNode(n : Node, prefix : PrefixPath) : Unit = {
    mapOfNodeState(n.name) match {
      case Unvisited ⇒
        //store the prefix
        mapOfNodePrefixes += n.name → prefix
        mapOfNodeState += n.name → StillVisiting
        val prefixAfter = prefix.addNonMutable(n.name)
        for (nbr ← n.nbrs) {
          dfsOnNode(mapOfNodes(nbr), prefixAfter)
        }
        mapOfNodeState += n.name → Visited
      case StillVisiting ⇒
        val cyclePossible = prefix.subtract(mapOfNodePrefixes(n.name))
        if (maxTour < cyclePossible) maxTour = cyclePossible
      case Visited ⇒
    }
  }
}

class ProblemB(inp : String) {
  processInput

  def processInput : Unit = {
    val graph = new Graph(Integer.parseInt(inp.split(("\n"))(0)))

    graph.addNode(1, List(2, 3))
    graph.addNode(2, List(1, 3))
    graph.addNode(3, List(1, 2))
    var cnt = 4
    inp.split(("\n")).drop(1) foreach { str ⇒
      val nbrs = str.split(" ") map { Integer.parseInt(_) } toList

      graph.addNode(cnt, nbrs)
      cnt = cnt + 1
    }
    println(graph.findMaxTour)
  }

}

class ProblemBTesting extends FunSuite {
  test("a") {
    val s =
      """5
        |1 2
        |2 1""".stripMargin
    new ProblemB(s)

  }

  test("b") {
    val s =
      """6
        |1 2
        |1 4
        |4 5""".stripMargin
    new ProblemB(s)

  }
  test("c") {
    val s =
      """560
        |2 1
        |1 2
        |3 1
        |4 1
        |1 2
        |3 2
        |1 2
        |1 3
        |2 5
        |8 2
        |6 3
        |2 1
        |12 2
        |1 6
        |1 4
        |2 1
        |1 2
        |16 2
        |1 2
        |3 11
        |12 16
        |2 3
        |13 2
        |6 1
        |8 13
        |1 3
        |2 12
        |1 3
        |1 3
        |2 25
        |3 14
        |3 1
        |1 6
        |2 4
        |2 1
        |3 25
        |13 28
        |26 2
        |6 17
        |1 31
        |2 25
        |40 28
        |6 36
        |10 1
        |21 16
        |25 3
        |3 1
        |6 42
        |1 2
        |1 3
        |24 16
        |1 3
        |13 28
        |1 36
        |19 1
        |3 29
        |13 8
        |13 40
        |1 27
        |2 38
        |63 38
        |1 2
        |39 3
        |3 1
        |5 1
        |23 3
        |2 1
        |3 1
        |2 26
        |23 11
        |72 2
        |58 1
        |3 1
        |1 2
        |2 9
        |5 12
        |12 30
        |2 8
        |1 4
        |1 3
        |32 1
        |2 1
        |3 14
        |14 86
        |3 50
        |1 85
        |2 20
        |1 3
        |1 85
        |3 1
        |45 28
        |86 87
        |2 37
        |2 1
        |37 96
        |3 39
        |2 1
        |1 2
        |74 2
        |1 55
        |1 2
        |2 1
        |63 2
        |3 1
        |1 3
        |8 1
        |2 1
        |17 42
        |42 111
        |1 6
        |2 3
        |1 3
        |1 3
        |96 2
        |1 89
        |39 66
        |69 3
        |2 1
        |2 1
        |1 3
        |1 68
        |3 1
        |1 2
        |2 1
        |107 3
        |3 1
        |3 1
        |73 23
        |2 1
        |2 81
        |2 1
        |1 2
        |3 1
        |113 1
        |2 121
        |132 1
        |3 1
        |2 1
        |1 3
        |2 1
        |92 1
        |123 1
        |3 1
        |3 1
        |77 2
        |2 4
        |1 3
        |2 96
        |59 3
        |3 1
        |67 3
        |1 3
        |3 1
        |13 40
        |4 2
        |3 91
        |2 1
        |1 107
        |6 17
        |3 1
        |2 1
        |1 108
        |2 52
        |5 79
        |157 13
        |1 3
        |38 1
        |1 3
        |2 1
        |76 3
        |87 14
        |45 28
        |15 1
        |2 1
        |3 150
        |2 1
        |83 1
        |2 1
        |181 1
        |29 3
        |1 58
        |3 155
        |12 80
        |2 3
        |129 1
        |188 1
        |50 1
        |1 3
        |37 98
        |1 3
        |3 25
        |114 3
        |26 2
        |196 2
        |1 3
        |1 163
        |45 175
        |164 2
        |5 167
        |42 51
        |25 44
        |204 44
        |3 1
        |86 95
        |2 1
        |69 3
        |3 209
        |161 1
        |37 98
        |98 212
        |12 5
        |3 1
        |13 26
        |3 1
        |2 1
        |59 29
        |1 3
        |196 197
        |2 135
        |1 116
        |197 221
        |104 1
        |3 1
        |135 222
        |1 2
        |85 92
        |18 1
        |1 165
        |71 3
        |1 142
        |5 202
        |216 13
        |145 123
        |174 14
        |14 86
        |1 2
        |2 1
        |1 233
        |89 118
        |156 3
        |2 12
        |180 83
        |40 157
        |1 132
        |2 1
        |1 248
        |2 148
        |111 17
        |4 7
        |46 36
        |2 1
        |1 2
        |1 3
        |1 2
        |2 1
        |20 2
        |58 1
        |1 126
        |2 1
        |1 3
        |1 3
        |130 3
        |1 258
        |3 1
        |267 1
        |213 98
        |116 223
        |3 1
        |3 1
        |1 208
        |1 3
        |198 3
        |3 1
        |29 183
        |1 3
        |2 1
        |39 3
        |2 1
        |1 124
        |124 282
        |148 2
        |89 118
        |29 183
        |3 1
        |2 122
        |126 2
        |3 1
        |3 1
        |45 175
        |1 2
        |2 1
        |3 1
        |1 2
        |200 45
        |63 38
        |92 144
        |25 2
        |104 2
        |1 3
        |50 190
        |3 191
        |84 32
        |32 84
        |13 216
        |1 2
        |1 3
        |2 121
        |181 2
        |2 311
        |1 3
        |147 1
        |15 2
        |2 177
        |1 2
        |1 3
        |241 233
        |20 259
        |2 1
        |128 3
        |2 201
        |275 3
        |126 289
        |3 1
        |3 1
        |2 239
        |1 3
        |1 3
        |1 2
        |40 246
        |1 3
        |141 1
        |3 193
        |3 330
        |336 330
        |196 197
        |1 2
        |326 1
        |2 1
        |216 235
        |3 1
        |49 3
        |1 29
        |3 1
        |186 80
        |1 125
        |126 2
        |70 1
        |317 2
        |10 1
        |1 254
        |317 2
        |29 3
        |66 3
        |1 3
        |21 48
        |1 302
        |144 1
        |3 115
        |1 247
        |2 106
        |1 3
        |287 3
        |3 107
        |1 82
        |1 155
        |3 193
        |3 1
        |3 1
        |1 2
        |372 2
        |155 368
        |2 1
        |4 18
        |305 84
        |200 175
        |227 135
        |3 1
        |1 2
        |2 381
        |232 3
        |2 1
        |2 1
        |380 3
        |147 314
        |232 3
        |28 56
        |1 107
        |1 2
        |63 106
        |3 1
        |2 382
        |2 19
        |2 1
        |1 220
        |1 3
        |1 3
        |49 25
        |159 91
        |91 401
        |7 1
        |1 2
        |38 170
        |1 2
        |44 204
        |3 215
        |2 1
        |14 87
        |283 282
        |2 1
        |172 1
        |2 1
        |155 3
        |20 2
        |121 2
        |1 3
        |3 356
        |325 126
        |66 39
        |120 69
        |3 1
        |3 1
        |1 142
        |174 237
        |177 316
        |17 1
        |1 2
        |344 49
        |59 3
        |2 177
        |266 1
        |2 1
        |122 288
        |105 2
        |52 2
        |38 63
        |227 222
        |3 35
        |325 420
        |394 2
        |166 52
        |2 33
        |444 33
        |1 390
        |2 279
        |1 2
        |9 3
        |2 312
        |119 66
        |3 215
        |77 148
        |148 453
        |2 293
        |119 66
        |1 3
        |1 2
        |248 2
        |210 3
        |106 63
        |1 2
        |181 2
        |17 162
        |2 1
        |2 1
        |1 418
        |1 2
        |2 1
        |1 458
        |3 1
        |2 1
        |73 131
        |1 89
        |1 3
        |1 2
        |364 3
        |378 175
        |167 5
        |248 2
        |2 395
        |13 168
        |68 124
        |16 24
        |2 1
        |52 437
        |3 1
        |345 1
        |444 2
        |1 372
        |3 1
        |1 3
        |376 18
        |1 2
        |1 228
        |1 208
        |47 1
        |2 248
        |1 3
        |499 3
        |247 362
        |141 2
        |215 408
        |323 2
        |1 3
        |364 1
        |446 1
        |168 13
        |343 1
        |45 297
        |1 132
        |511 132
        |415 3
        |118 89
        |1 47
        |1 3
        |188 1
        |74 72
        |3 1
        |2 3
        |520 3
        |1 3
        |17 111
        |2 1
        |3 1
        |1 170
        |2 1
        |492 1
        |209 210
        |3 1
        |2 416
        |2 1
        |82 1
        |237 174
        |368 374
        |341 2
        |191 3
        |37 2
        |329 3
        |42 112
        |314 147
        |452 215
        |1 3
        |1 2
        |15 1
        |2 538
        |2 1
        |319 241
        |3 330
        |13 60
        |2 1
        |2 1
        |1 3
        |13 28
        |318 3
        |3 555
        |1 530
        |48 16
        |526 170
        |75 1""".stripMargin
    new ProblemB(s)
  }

}
