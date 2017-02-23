package com.nsinha.graph.algorithms.GraphTraversalsAll

import com.nsinha.graph.interfaces.Graph.NodeTrait

/** x1
  * x2     x3
  * x4 x1  x2 x5
  *
  * x1
  * x2      x3
  * x4  x5
  * x5    x6
  *
  * How a bfs might proceeed:
  * bfs(x1,[]) = bfs(que(x1),[],res[])) = bfs(que(x2 x3),[x1],[x1])
  * = bfs(que(x3 x4 x1),[x1x2],[x1 x2])= bfs(que(x4 x1 x2 x5) ,[x1 x2 x3],[x1 x2 x3])=
  * = bfs(que(x1 x2 x5),[x1 x2 x3 x4],[x1 x2x3x4]) = bfs(q(x2 x5), [x1 x2 x3 x4),[x1 x2 x3 x4])=
  * =bfs(q(x5) ,[x1 x2 x3 x4],[x1 x2 x3 x4]) = ...
  * bfs(xn, context) = x \in Context | phi
  *      else |que + x1 x2
  * 1.bfs(xn,context) => bfs(x,[]) ^^ bfstree(x) \dontintesect context
  * 2.bfs(xn,context) => bfs(x,[]) == bfs(xn,context) ++ bfs(m,[]) \foreach m in context ^^ bfs(x,[]) ^^ bfstree(x) \intersect context
  * once done with a node:
  * check the context que is fifo order and process the bfs of those nodes again for placeholders and use 2. The queue order of context
  * gurantees that  any pre occuring element will not depend on post occuring node as post occuring node if occurs in bfs of pre node
  * will be expanded once at some stage of pre node expansion.Thus later  multiple occurence of post node can evaluate to nil without any issue.
  * after this step use 1 flatten anything that has non-zero context. There can not be anyone remaining with non-zero context as if a
  * bfs(xn,contest) cant be flattened then \there exists y st y \is mem of bfstree(xn) and bfs(y,[]) does not exist.
  * ->Let y occur befor x in bfs(x1). As y has not expanded fully there must exist z that occurs prior to y ad adinfinitum we can show that xn,[]
  * cant exist. Not possible
  * ->let y occur after x in bfs(x1). for bfs(y,context) not to be expanded fully
  *
  */
/** Created by nsinha on 2/7/17.
  */

case class NodeTraitWithTravesralContext(nodeTrait : NodeTrait, context : List[String]) {
  def getNode : Option[NodeTrait] = if (context.contains(nodeTrait.name)) None else Option(nodeTrait)
  def getContext : List[String] = context
  def isUndetermined : Boolean = (context.size != 0)

}

/** @param g
  * @tparam A
  *
  * class Bfs[A](g: GraphTrait[A]) {
  * val cache: mutable.Map[String, List[NodeTrait]] = mutable.Map()
  *
  * def doBfs(): List[TreeTrait[A]] = {
  *
  * val res = for (node <- g.nodes) yield {
  * doBfsOnNode(node, new mutable.Queue[String]())
  * }
  *
  * res
  * }
  *
  * def doBfsOnNode(node: NodeTraitWithTravesralContext, context: mutable.Queue[String]): Option[List[NodeTraitWithTravesralContext]] = {
  * cache.get(node.name) match {
  * case Some(cached) => Option(cached)
  * case None =>
  * if (context.contains(node.name)) {
  * None
  * } else {
  * //whose context
  * context.enqueue(node.name)
  * val que = new mutable.Queue[NodeTrait]()
  * que.enqueue(node)
  * doBfsOnQueue(que, context)
  * }
  * }
  * }
  *
  * def doBfsOnQueue(que: mutable.Queue[NodeTraitWithTravesralContext], context: mutable.Queue[String]): Option[List[NodeTraitWithTravesralContext]] = {
  * if(que.nonEmpty) {
  * val curnode = que.dequeue()
  * }
  *
  * }
  *
  * }
  *
  */

