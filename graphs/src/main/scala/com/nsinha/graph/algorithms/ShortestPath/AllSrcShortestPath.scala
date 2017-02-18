package com.nsinha.graph.algorithms.ShortestPath

import com.nsinha.graph.interfaces.Common.RingElem
import com.nsinha.graph.interfaces.Graph.GraphTrait

import scala.collection.mutable

/** Created by nsinha on 2/17/17.
  */
class AllSrcShortestPath[A <: RingElem[A]](g : GraphTrait[A])(implicit Zero : A) {
  val diaryOfPaths = mutable.Map[(String, String), (List[String], A)]()
  val diaryOfPathsInLastUpdate1 = mutable.Map[(String, String), (List[String], A)]()
  val diaryOfPathsInLastUpdate2 = mutable.Map[(String, String), (List[String], A)]()
  var complexity = 0

  def getAllShortestPaths : List[(String, String, List[String], A)] = {
    //initialize the diary of paths and keep doing the steps till you get a stable condition
    //stable condition is reached when diaryOfPathsInLastUpdate is empty
    //if the stability is not achieved by V*girth(G) < = V^2 such steps then this graph has
    //negartive cycle loop
    init()

    while (diaryOfPathsInLastUpdate1.nonEmpty || diaryOfPathsInLastUpdate2.nonEmpty) {
      //min(sum(P(a,b), P(b,c))) => P(a,b,c)
      val diaryOfPathsInLastUpdate = if (diaryOfPathsInLastUpdate1.nonEmpty) diaryOfPathsInLastUpdate1 else diaryOfPathsInLastUpdate2
      val diaryOfPathsToUpdate = if (!diaryOfPathsInLastUpdate1.nonEmpty) diaryOfPathsInLastUpdate1 else diaryOfPathsInLastUpdate2
      for (abPW ← diaryOfPathsInLastUpdate) {
        val (a, b) = abPW._1
        val (pathNodesAb, pathWeightAb) = abPW._2
        //consider immediate neighbors of b which is domain of c
        val clist = g.getEdgeWithSrc(b)
        for (cedge ← clist) {
          complexity = complexity + 1
          val c = cedge.name._2
          val pathWeightBc = cedge.weight.getWeight
          if (diaryOfPaths.contains((a, c))) {
            val (prevPathAC, weightAToC) = diaryOfPaths((a, c))
            if (weightAToC.compare(pathWeightAb.`+`(pathWeightBc)) > 0) {
              diaryOfPathsToUpdate += ((a, c) → (pathNodesAb :+ (c), pathWeightAb.`+`(pathWeightBc)))
              diaryOfPaths += ((a, c) → (pathNodesAb :+ (c), pathWeightAb.`+`(pathWeightBc)))
            }
          }
          else {
            diaryOfPathsToUpdate += ((a, c) → (pathNodesAb :+ (c), pathWeightAb.`+`(pathWeightBc)))
            diaryOfPaths += ((a, c) → (pathNodesAb :+ (c), pathWeightAb.`+`(pathWeightBc)))
          }
        }
      }
      diaryOfPathsInLastUpdate.clear
    }

    println(s"nodes size = ${g.nodes.size} complexity = $complexity")
    diaryOfPaths.foldLeft(List[(String, String, List[String], A)] ()) ((z, el) ⇒ z :+ (el._1._1, el._1._2, el._2._1, el._2._2))
  }

  def init() = {
    g.edges map { edge ⇒
      val weight = edge.weight.getWeight
      val l = List(edge.name._1, edge.name._2)
      diaryOfPaths += (edge.name → (List(edge.name._1, edge.name._2), weight))
      diaryOfPathsInLastUpdate1 += (edge.name → (List(edge.name._1, edge.name._2), weight))
    }
    g.nodes foreach { node ⇒
      diaryOfPaths += ((node.name, node.name) → (List(node.name, node.name), Zero))
    }
  }

}
