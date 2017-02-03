package com.nsinha.graph.utils.dot

import com.nsinha.graph.interfaces._

import scala.collection.mutable
import scala.io.Source

/**
  * Created by nsinha on 2/2/17.
  */
trait DotReaderToGraphTrait[A] {
  def readFileIntoGraph(fileName: String): GraphTrait[A]
}


class DotReaderImpl[A] extends DotReaderToGraphTrait[A] {
  val regexNode = "([a-zA-Z0-9]+).*\\[(.*)\\]".r
  val edgeNode = "([a-zA-Z0-9]+)[ ]*->[ ]*([a-zA-Z0-9]+)".r
  val nodeToNodes: mutable.Map[String, mutable.Set[String]] = mutable.Map()
  //new Node[OpaqeClass](s"n$i", Math.cos(Math.PI * 2 * i / totalNodes), Math.sin(Math.PI * 2 * i / totalNodes), l map (x => s"n$x"))

  override def readFileIntoGraph(fileName: String): GraphTrait[A] =  {
    //a very procedural way of reading a graph. I intend to use a parser in long run.  I beseech you Dont judge me please or I may judge you too.
    //assuming very fixed formatting for now
    val src: Source = Source.fromFile(fileName)
    var ignore = false

    for (line <- src.getLines()) {
      if(ignore) {

      } else {
        //does it contain ->
        if(line.contains("{")|| line.contains("}")) {} else if (line.contains("->")) {
          //a edge
          val edgeNode(n1,n2) = line
          nodeToNodes.get(n1) match {
            case None => nodeToNodes.+=((n1,mutable.Set(n2)))
            case Some(x) => x.+=(n2)
          }
          nodeToNodes.get(n2) match {
            case None => nodeToNodes.+=((n1, mutable.Set()))
            case _ =>
          }
        } else {
          // a node
          val regexNode(nodeName,attrs) = line
          nodeToNodes.get(nodeName) match {
            case None => nodeToNodes.+=((nodeName, mutable.Set()))
            case _ =>
          }
        }
      }
    }

    val nodes = nodeToNodes zip Range(0, nodeToNodes.size) map {x =>
      new Node[A](x._1._1, Math.cos(Math.PI * 2 * x._2 / nodeToNodes.size), Math.sin(Math.PI * 2 * x._2 / nodeToNodes.size), x._1._2.toList)
    } toList

    new Graph[A](_nodes = nodes, _isDirected = true, (x: (String, String), y: Int) => new Weight[A] {
      override def getWeight: A = ???
    })
  }
}
