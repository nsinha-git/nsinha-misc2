package com.nsinha.graph.interfaces.Ops

import com.nsinha.graph.algorithms.Bipartite.Bipartite
import com.nsinha.graph.algorithms.ConnectedComponent
import com.nsinha.graph.algorithms.FullyConnectedComponent.Fcc
import com.nsinha.graph.algorithms.StronglyConnectedComponent.Scc
import com.nsinha.graph.algorithms.ToplogicalOrdering.TopologicalOrdering
import com.nsinha.graph.factories.GraphFactory
import com.nsinha.graph.interfaces.Common.Attribute
import com.nsinha.graph.interfaces.Graph._
import com.nsinha.graph.interfaces._
import com.nsinha.graph.utils.ExternalProcess
import com.nsinha.library.{MonadicResult, MonadicResultImpl}

import scala.collection.mutable
import scala.reflect.io.File

/** Created by nsinha on 1/31/17.
  */
trait GraphOpsTrait[A] {
  type G = GraphTrait[A]
  val g : G

  def color(edgeName : (String, String), g : G) : Int = 1

  def bfsTree(nodeName : String) : Option[TreeTrait[A]] = {
    val node = g.getNode(nodeName)
    val que = new mutable.Queue[NodeTrait]()
    que.enqueue(node)
    val newNodesMonad = bfsTreeInt(que, mutable.Set(), nodeName)(new MonadicResultImpl[List[NodeTrait], Int](Nil, 0)(() ⇒ Nil, () ⇒ 0))(false)
    val newG = g.deepClone(newNodesMonad._1.getResult)
    println(s"We spent ${newNodesMonad._1.getState}  in BFS")
    Option(new Tree[A](node, newG))
  }

  def bfsTreeCycle(nodeName : String) : Boolean = {
    val node = g.getNode(nodeName)
    val que = new mutable.Queue[NodeTrait]()
    que.enqueue(node)
    val newNodesMonad = bfsTreeInt(que, mutable.Set(), nodeName)(new MonadicResultImpl[List[NodeTrait], Int](Nil, 0)(() ⇒ Nil, () ⇒ 0))(false)
    newNodesMonad._2
  }

  def bfsTreeAll() : List[TreeTrait[A]] = {
    val allBfs = for (node ← g.nodes) yield {
      bfsTree(node.name)
    }

    allBfs.foldLeft(List[TreeTrait[A]]()) {
      (z, el) ⇒
        el match {
          case None    ⇒ z
          case Some(y) ⇒ z :+ y
        }
    }
  }

  def dfsTree(nodeName : String) : Option[TreeTrait[A]] = {
    val node = g.getNode(nodeName)
    val newNodesMonad = dfsTreeInt(node, mutable.Set())(new MonadicResultImpl[List[NodeTrait], Int](Nil, 0)(() ⇒ Nil, () ⇒ 0))
    val newG = g.deepClone(newNodesMonad.getResult)
    println(s"We spent ${newNodesMonad.getState}  in DFS")
    Option(new Tree[A](node, newG))
  }

  def dfsTreeAll() : List[TreeTrait[A]] = {
    val allDfs = for (node ← g.nodes) yield {
      dfsTree(node.name)
    }

    allDfs.foldLeft(List[TreeTrait[A]]()) {
      (z, el) ⇒
        el match {
          case None    ⇒ z
          case Some(y) ⇒ z :+ y
        }
    }
  }

  def addAttribute(name : String, attribute : Attribute, nodeName : String, g : G) = ???

  def delAttribute(name : String, attibute : String, nodeName : String, g : G) = ???

  def isFullyConnected : Boolean = {
    val areNodesConnected = g.nodes map { n1 ⇒
      val filtered = g.nodes filter (_.name != n1.name) map (_.name)

      filtered.foldLeft(true) { (z, el) ⇒
        if (n1.children().contains(el)) {
          z | true
        }
        else {
          z & false
        }
      }
    }
    areNodesConnected.reduce((x, y) ⇒ x & y)
  }

  def getFullyConnectedComponents : List[ConnectedComponent[A]] = {
    Fcc(g).scc()
  }

  def transpose : GraphTrait[A] = {
    val allnodes = {
      g.nodes map (_.name)
    }.toSet

    val nodes = for (node ← g.nodes) yield {
      val children = node.children().toSet
      val transposedChildren = allnodes diff (children + node.name) toList

      node.deepClone(transposedChildren)
    }
    GraphFactory.createGraphOfOpaques(nodes, true, g.getWeightFn)
  }

  def getStronglyConnectedComponents() : List[ConnectedComponent[A]] = {
    Scc(g).scc
  }

  def bipartite() : (List[String], List[String]) = {
    { new Bipartite[A](g) }.bipart
  }

  def doesHaveCycles() : Boolean = {
    var cycles = false
    for (node ← g.nodes) {
      cycles = cycles | bfsTreeCycle(node.name)
    }
    cycles
  }

  def topologicalSort() : List[String] = {
    val newTopo = new TopologicalOrdering[A](g)
    newTopo.topoOrder()
  }

  def createNewGraph(fn : GraphCreateFn[A]) : GraphTrait[A] = ???

  def fold[B, C](z : B)(fn : (B, NodeTrait) ⇒ GraphTrait[C]) = ???

  def createSpaceEmbeddedGnuplotData(fileName : String = "/tmp/graph.dat") = {
    val f = new File(new java.io.File(fileName))
    val writer = f.bufferedWriter(false)
    //go to each node and write down a 2 lines for each of its neigbors with a blank line
    writer.append("#x #y #color #name \n")

    g.nodes flatMap { node ⇒
      g.getEdgeWithSrc(node.name)
    } map { x ⇒
      val n1 = g.getNode(x.name._1)
      val n2 = g.getNode(x.name._2)
      writer.append(n1.x+" "+n1.y+" "+color(x.name, g)+" "+n1.name+"\n")
      writer.append(n2.x+" "+n2.y+" "+color(x.name, g)+" "+n2.name+"\n\n")
    }
    writer.close()
    //call a gnuplot process
    ExternalProcess(Seq("/usr/local/bin/gnuplot", "-c", "/Users/nsinha/mygithubs/nsinha-misc2/graphs/src/main/resources/run.gnuplot"))
    ExternalProcess(Seq("/usr/bin/open", "/tmp/graphOp.ps"))
  }

  def printGraphDot(fileName : String = "/tmp/graph.dot") = {
    val f = new File(new java.io.File(fileName))
    val writer = f.bufferedWriter(false)
    //go to each node and write down a 2 lines for each of its neigbors with a blank line
    writer.append("digraph d {\n")

    g.nodes map {
      n ⇒ writer.append(n.name+" [label="+n.name+";"+"]\n")
    }
    g.nodes flatMap { node ⇒
      g.getEdgeWithSrc(node.name)
    } map { x ⇒
      val n1 = g.getNode(x.name._1)
      val n2 = g.getNode(x.name._2)
      writer.append(n1.name+"->"+n2.name+"\n")
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
    g.nodes map { x ⇒ print(s"${x.name}:${x.x},${x.y}") }
    println()
    println("Edges:")
    g.edges map { e ⇒ print(s"${e.name}") }
  }

  private def dfsTreeInt(node : NodeTrait, visited : mutable.Set[String])(nodesMonads : MonadicResult[List[NodeTrait], Int]) : MonadicResult[List[NodeTrait], Int] = {
    if (visited.contains(node.name)) {
      return nodesMonads
    }
    else {
      visited.+=(node.name)
      val newNode = node.deepClone(node.children() filter (x ⇒ if (visited.contains(x)) false else true))
      val newNodesMonad = nodesMonads map ((x, y) ⇒ (x :+ newNode, y + 1))
      val allChildDfs = node.children() map { child ⇒ dfsTreeInt(g.getNode(child), visited)(new MonadicResultImpl[List[NodeTrait], Int](Nil, 0)(() ⇒ Nil, () ⇒ 0)) }
      allChildDfs.foldLeft(newNodesMonad) { (z, el) ⇒
        z flatMap ((x, y) ⇒ new MonadicResultImpl[List[NodeTrait], Int](x ++ el.getResult, y + el.getState)(() ⇒ Nil, () ⇒ 0))
      }
    }
  }

  private def bfsTreeInt(que : mutable.Queue[NodeTrait], visited : mutable.Set[String], contextNode : String)(nodesMonads : MonadicResult[List[NodeTrait], Int])(cyclesPresent : Boolean) : (MonadicResult[List[NodeTrait], Int], Boolean) = {
    if (que.nonEmpty) {
      val curNode = que.dequeue()
      if (visited.contains(curNode.name)) {
        if (contextNode == curNode.name)
          bfsTreeInt(que, visited, contextNode)(nodesMonads)(true)
        else
          bfsTreeInt(que, visited, contextNode)(nodesMonads)(cyclesPresent)
      }
      else {
        visited.+=(curNode.name)
        val newNode = curNode.deepClone(curNode.children() filter (x ⇒ if (visited.contains(x)) false else true))
        val newNodesMonad = nodesMonads map ((x, y) ⇒ (x.:+(newNode), y + 1))
        curNode.children() map (x ⇒ g.getNode(x)) foreach (x ⇒ que.enqueue(x))
        var cyclesNew = false
        val y = newNodesMonad flatMap { (x, y) ⇒
          val z = bfsTreeInt(que, visited, contextNode)(new MonadicResultImpl[List[NodeTrait], Int](x, y)(() ⇒ Nil, () ⇒ 0))(cyclesPresent)
          cyclesNew = cyclesNew | z._2
          z._1
        }
        (y, cyclesNew | cyclesPresent)
      }
    }
    else {
      (nodesMonads, cyclesPresent)
    }
  }
}

