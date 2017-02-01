package com.nsinha.graph.interfaces


import com.nsinha.graph.utils.ExternalProcess

import scala.collection.mutable
import scala.reflect.io.File


/**
  * Created by nsinha on 1/31/17.
  */
trait GraphOpsTrait[A] {
  type G = GraphTrait[A]

  def color(edgeName: (String, String), g: G): Int = 1

  def bfsTree(nodeName: String, g: G): Option[TreeTrait[A]] = {
    val node = g.getNode(nodeName)

    val que = new mutable.Queue[NodeTrait[A]]()
    que.enqueue(node)

    val newNodes  = bfsTreeInt(que,g, Set())(Nil)
    val newG = g.deepClone(newNodes)
    Option(new Tree[A] (node, newG))
  }

  private def bfsTreeInt(que: mutable.Queue[NodeTrait[A]],g: GraphTrait[A], visited: Set[String])(nodes: List[NodeTrait[A]]): List[NodeTrait[A]] = {
    if(que.size == 0){
      nodes
    } else {
      val curNode = que.dequeue()
      if (visited.contains(curNode.name)){
        bfsTreeInt(que, g, visited)(nodes)
      } else {
        val newVisited = visited.+(curNode.name)
        curNode.children() map (x => g.getNode(x)) foreach (x => que.enqueue(x))
        val newNode = curNode.deepClone(curNode.children() filter(x => if (newVisited.contains(x)) false else true) )

        val newNodes = nodes.+:(newNode)
        bfsTreeInt(que, g, newVisited)(newNodes)
      }
    }
  }

  def dfsTree(nodeName: String, g: G): Option[TreeTrait[A]] = ???

  def addAttribute(name: String, attribute: Attribute, nodeName: String, g: G) = ???
  def delAttribute(name: String, attibute: String, nodeName: String, g: G) = ???

  def isFullyConnected(g: G): Boolean = ???
  def getStronglyConnectedComponenets(g: G): List[GraphTrait[A]] = ???
  def getWeaklyConnectedComponents(g: G): List[GraphTrait[A]] = ???

  def topologicalSort(g: G): List[TreeTrait[A]] = ???

  def createNewGraph(fn: GraphCreateFn[A], g: G): GraphTrait[A] = ???

  def fold[B,C] (z:B)(g: GraphTrait[A]) (fn: (B, NodeTrait[A]) => GraphTrait[C]) = ???

  def createSpaceEmbeddedGnuplotData(g: G, fileName: String = "/tmp/graph.dat") =  {
    val f = new File(new java.io.File(fileName))
    val writer = f.bufferedWriter(false)
    //go to each node and write down a 2 lines for each of its neigbors with a blank line
    writer.append("#x #y #color #name \n")

    g.nodes flatMap {node =>
     g.getEdgeWithSrc(node.name)
    } map {x =>
      val n1 = g.getNode(x.name._1)
      val n2 = g.getNode(x.name._2)
      writer.append(n1.x + " " + n1.y + " " + color(x.name,g) + " " + n1.name + "\n" )
      writer.append(n2.x + " " + n2.y + " " + color(x.name,g) + " " + n2.name + "\n\n" )
    }

    writer.close()
    //call a gnuplot process
    ExternalProcess(Seq("/usr/local/bin/gnuplot","-c","/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/main/resources/run.gnuplot"))

    ExternalProcess(Seq("/usr/bin/open", "/tmp/graphOp.ps"))
  }

  def printGraph(g: G) = {
    println(s"No of nodes= ${g.nodes.length}")
    println(s"No of edges= ${g.edges.length}")
    println("Nodes:")
    g.nodes map {x =>  print(s"${x.name}:${x.x},${x.y}")}
    println()
    println("Edges:")
    g.edges map {e => print(s"${e.name}")}
  }
}



trait OrderedGraphOps[A <: Ordered[A]] extends GraphOpsTrait[A]{
  def minSpanningTree(g: G): OrderedTreeTrait[A] = ???
}

trait NumericGrapOps[A <: Numeric[A] with Ordered[A]] extends  OrderedGraphOps[A] {
  def findMinDistSrcDest(src: String, dest: String, g: G): Option[A]
  def findMinDistForEachSrcDest(g: G): List[(String, String, Option[A])]
  def findMaxFlowSrcDest(src: String, dest: String, g: G): A
  def findMaxFlowForEachSrcDest(g: G): List[(String, String, A)]
}

trait TreeOpsTrait[A] extends GraphOpsTrait[A] {
  def preOrderList: List[NodeTrait[A]]
  def inOrderList: List[NodeTrait[A]]
  def postOrderList: List[NodeTrait[A]]
}
