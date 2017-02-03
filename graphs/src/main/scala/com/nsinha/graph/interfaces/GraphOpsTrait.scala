package com.nsinha.graph.interfaces

import com.nsinha.graph.utils.ExternalProcess
import com.nsinha.library.{MonadicResult, MonadicResultImpl}

import scala.collection.mutable
import scala.reflect.io.File

/**
 * Created by nsinha on 1/31/17.
 */
trait GraphOpsTrait[A] {
  type G = GraphTrait[A]
  val g: G

  def color(edgeName: (String, String), g: G): Int = 1

  def bfsTree(nodeName: String): Option[TreeTrait[A]] = {
    val node = g.getNode(nodeName)
    val que = new mutable.Queue[NodeTrait[A]]()
    que.enqueue(node)
    val newNodesMonad = bfsTreeInt(que, mutable.Set())(new MonadicResultImpl[List[NodeTrait[A]], Int](Nil, 0)(() => Nil, () => 0))
    val newG = g.deepClone(newNodesMonad.getResult)
    println(s"We spent ${newNodesMonad.getState}  in BFS")
    Option(new Tree[A](node, newG))
  }

  def dfsTree(nodeName: String): Option[TreeTrait[A]] = {
    val node = g.getNode(nodeName)
    val newNodes = dfsTreeInt(node, mutable.Set())(Nil)
    val newG = g.deepClone(newNodes)
    Option(new Tree[A](node, newG))
  }

  def addAttribute(name: String, attribute: Attribute, nodeName: String, g: G) = ???

  def delAttribute(name: String, attibute: String, nodeName: String, g: G) = ???

  def isFullyConnected: Boolean = ???

  def getStronglyConnectedComponents: List[GraphTrait[A]] = ???

  def getWeaklyConnectedComponents(g: G): List[GraphTrait[A]] = ???

  def topologicalSort(): List[TreeTrait[A]] = ???

  def createNewGraph(fn: GraphCreateFn[A]): GraphTrait[A] = ???

  def fold[B, C](z: B)(fn: (B, NodeTrait[A]) => GraphTrait[C]) = ???

  def createSpaceEmbeddedGnuplotData(fileName: String = "/tmp/graph.dat") = {
    val f = new File(new java.io.File(fileName))
    val writer = f.bufferedWriter(false)
    //go to each node and write down a 2 lines for each of its neigbors with a blank line
    writer.append("#x #y #color #name \n")

    g.nodes flatMap { node =>
      g.getEdgeWithSrc(node.name)
    } map { x =>
      val n1 = g.getNode(x.name._1)
      val n2 = g.getNode(x.name._2)
      writer.append(n1.x + " " + n1.y + " " + color(x.name, g) + " " + n1.name + "\n")
      writer.append(n2.x + " " + n2.y + " " + color(x.name, g) + " " + n2.name + "\n\n")
    }

    writer.close()
    //call a gnuplot process
    ExternalProcess(Seq("/usr/local/bin/gnuplot", "-c", "/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/main/resources/run.gnuplot"))

    ExternalProcess(Seq("/usr/bin/open", "/tmp/graphOp.ps"))
  }

  def printGraphDot(fileName: String = "/tmp/graph.dot") = {
    val f = new File(new java.io.File(fileName))
    val writer = f.bufferedWriter(false)
    //go to each node and write down a 2 lines for each of its neigbors with a blank line
    writer.append("digraph d {\n")

    g.nodes map {
      n => writer.append(n.name + " [label=" + n.name + ";" + "]\n")
    }
    g.nodes flatMap { node =>
      g.getEdgeWithSrc(node.name)
    } map { x =>
      val n1 = g.getNode(x.name._1)
      val n2 = g.getNode(x.name._2)
      writer.append(n1.name + "->" + n2.name + "\n")
    }

    writer.append("}\n")
    writer.close()
    //call a gnuplot process
    ExternalProcess(Seq("/usr/local/bin/dot", "-Tpng", "-O", fileName))

    ExternalProcess(Seq("/usr/bin/open", s"${fileName}.png"))
  }

  def printGraph = {
    println(s"No of nodes= ${g.nodes.length}")
    println(s"No of edges= ${g.edges.length}")
    println("Nodes:")
    g.nodes map { x => print(s"${x.name}:${x.x},${x.y}") }
    println()
    println("Edges:")
    g.edges map { e => print(s"${e.name}") }
  }

  private def dfsTreeInt(node: NodeTrait[A], visited: mutable.Set[String])(nodes: List[NodeTrait[A]]): List[NodeTrait[A]] = {
    if (visited.contains(node.name)) {
      return nodes
    } else {
      visited.+=(node.name)
      val newNode = node.deepClone(node.children() filter (x => if (visited.contains(x)) false else true))
      val newNodes = nodes :+ newNode
      newNodes.++(node.children() flatMap { x => dfsTreeInt(g.getNode(x), visited)(Nil) })
    }
  }

  private def bfsTreeInt(que: mutable.Queue[NodeTrait[A]], visited: mutable.Set[String])(nodesMonads: MonadicResult[List[NodeTrait[A]], Int]): MonadicResult[List[NodeTrait[A]], Int] = {
    if (que.nonEmpty) {
      val curNode = que.dequeue()
      if (visited.contains(curNode.name)) {
        bfsTreeInt(que, visited)(nodesMonads)
      } else {
        visited.+=(curNode.name)
        val newNode = curNode.deepClone(curNode.children() filter (x => if (visited.contains(x)) false else true))
        val newNodesMonad = nodesMonads map ((x, y) => (x.:+(newNode), y + 1))
        curNode.children() map (x => g.getNode(x)) foreach (x => que.enqueue(x))
        newNodesMonad flatMap ((x, y) => bfsTreeInt(que, visited)(new MonadicResultImpl[List[NodeTrait[A]], Int](x, y)(() => Nil, () => 0)))
      }
    } else {
      nodesMonads
    }
  }
}

trait OrderedGraphOps[A <: Ordered[A]] extends GraphOpsTrait[A] {
  def minSpanningTree(g: G): OrderedTreeTrait[A] = ???
}

trait NumericGrapOps[A <: Numeric[A] with Ordered[A]] extends OrderedGraphOps[A] {
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
