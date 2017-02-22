package com.nsinha.graph.algorithms.MaxFlow

import com.nsinha.graph.interfaces.Common.{RingElem, Weight}
import com.nsinha.graph.interfaces.Graph.{GraphOrdered, GraphTrait, NodeTrait}

import scala.collection.mutable

/** Created by nsinha on 2/18/17.
  */
class MaxFlow[A <: RingElem[A]](_g : GraphTrait[A])(implicit zero: A) {
  val obnoxiusHighWeight = _g.edges.foldLeft(zero) {(z,el) =>
    z + el.weight.getWeight
  }
  private def do_dfs(gCur : GraphTrait[A], node : NodeTrait, curPath : List[String], mp : mutable.Map[String, (A, List[String])]) : mutable.Map[String, (A, List[String])] = {
    //curPath should always have node at its end
    //we will find the pathWeight for node at this time. We expect the oldPath to be present. Not only when we re starting
    val weightForCurPathOpt = mp.get(curPath.mkString("-")) map (_._1)
    node.children foreach { kid ⇒
      val kidNode = gCur.getNode(kid)
      if (mp.contains(curPath.mkString("-") + "-" + kid)) {
        //dont do nothing
      }
      else {
        if (curPath.contains(kid)) {
          //the node is already in path think abc-a. This is cycle. We will stop
        }
        else {
          val edge = {
            gCur.getEdgeWithSrc(node.name) filter (x ⇒ x.name._2 == kid)
          }.head
          val newWeight = weightForCurPathOpt match {
            case None => edge.weight.getWeight
            case Some(weightForCurPath) => if (weightForCurPath > edge.weight.getWeight) edge.weight.getWeight else weightForCurPath
          }
          if (newWeight.compare(zero) == 0) {
          }
          else {
            mp(curPath.mkString("-") + "-" + kid) = (newWeight, curPath :+ kid)
            do_dfs(gCur, kidNode, curPath :+ kid, mp)
          }

        }
      }
    }
    mp
  }

  def returnAllMinPathsFromSrctoDst(src : String, dest : String, gCur : GraphTrait[A]) = {
    val allPathsSrcToAll = do_dfs(gCur, gCur.getNode(src), List(src), mutable.Map[String, (A, List[String])](src -> (obnoxiusHighWeight, List(src))))
    val allPathsSrcToDest = allPathsSrcToAll filter { x ⇒
      val allNodes = x._1.split("-")
      allNodes.reverse.head == dest
    } filter {x => x._2 != zero}

    if (allPathsSrcToDest.nonEmpty) {
      val sortedAllPathSrcToDest = allPathsSrcToDest.toList.sortBy(x ⇒ x._2._1)
      val minPathWeight  = sortedAllPathSrcToDest.head._2._1
      //get all minPathWeightPaths
      val minPaths = sortedAllPathSrcToDest filter (x => x._2._1 == minPathWeight)
      minPaths map { minPath =>
        val newGraph = getNewGraphAfterAccountingForPathCut(gCur, minPath._2)
        (minPath, newGraph)
      }
    }
    else {
      Nil
    }
  }

  def getNewGraphAfterAccountingForPathCut(gCur : GraphTrait[A], pathWithWeight : (A, List[String])) : GraphTrait[A] = {
    //do a path travesral with given path and keep reducing the weight of edges. link weight may turns zero.
    val (weightToReduce, path) = pathWithWeight
    val gCloned = gCur
    val nodes = gCloned.nodes map (_.name)
    val edges = gCloned.edges
    val pathPairs : Set[(String, String)] = (path zip path.drop(1)) toSet
    val newedgesDict = edges map { edge ⇒
      if (pathPairs.contains(edge.name)) {
        edge.name → new Weight[A] {
          override val getWeight : A = edge.weight.getWeight - weightToReduce
        }
      }
      else {
        edge.name → edge.weight
      }
    } toMap

    GraphOrdered.createAGraph (nodes, edges, gCloned.isDirected, (x : (String, String), y : Int) ⇒ { newedgesDict(x) })
  }

  def getMaxFlow(src : String, dest : String) : Option[(A, List[(A, List[String])])] = {

    getMaxFlowInt(src, dest, _g, Nil)
  }

  def getMaxFlowInt(src : String, dest : String, gCur: GraphTrait[A], listOfPaths: List[(A, List[String])]) : Option[(A, List[(A,List[String])])] = {
    val branchesOfPossibleFlows = returnAllMinPathsFromSrctoDst(src, dest, gCur)
    val allOptions = {
      branchesOfPossibleFlows map { x => getMaxFlowInt(src, dest, x._2, List(x._1._2))} }.foldLeft(List[(A, List[(A, List[String])])]()){(z,el) =>
      el match {
        case None=> z
        case Some(elSome) =>
          z :+ elSome
      }
    }.sortBy(x => x._1)

    if (allOptions.isEmpty) {
      if (listOfPaths.isEmpty) {
        None
      } else {
        Option((listOfPaths.foldLeft(zero){ (z,el) =>
          z + el._1
        },listOfPaths))
      }
    } else {
      if (listOfPaths.isEmpty) {
        allOptions.headOption
      } else {
        val y = allOptions.head
        Option((listOfPaths.foldLeft(zero){ (z,el) =>
          z + el._1
        } + y._1, listOfPaths ++ y._2))
      }
    }

  }

}
