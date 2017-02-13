package com.nsinha.graph.algorithms.CommonAncestor

/** Created by nsinha on 2/12/17.
  */
trait Node {
  val name : String
  var parent : Option[Node] = None
  var children : List[Node] = Nil
  var knownTree : Tree
}

trait Tree {
  val rootNode : Node
}

trait Graph {
  var setOfTrees : Set[Tree]
  var mapOfNodes : Map[String, Node]

  def addANode(p : String, c : String) : Option[Tree] = {
    val parent = if (p == "") {
      null
    }
    else {
      mapOfNodes.get(p) match {
        case None ⇒
          val n = new Node {
            override val name : String = p
            override var knownTree : Tree = _
          }
          mapOfNodes = mapOfNodes + (p → n)
          n.knownTree = new Tree {
            val rootNode = n
          }
          setOfTrees = setOfTrees + n.knownTree
          n
        case Some(n) ⇒ n
      }
    }
    val child = if (c == "") {
      return None
    }
    else {
      mapOfNodes.get(c) match {
        case None ⇒
          val n = new Node {
            override val name : String = c
            override var knownTree : Tree = _
          }
          mapOfNodes = mapOfNodes + (c → n)
          n.knownTree = new Tree {
            val rootNode = n
          }
          setOfTrees = setOfTrees + n.knownTree
          n
        case Some(n) ⇒ n
      }
    }
    addANodeInt(parent, child)
  }

  def addANodeInt(parent : Node, child : Node) : Option[Tree] = {
    if (findCommonAncestor(parent, child) == null) {
      if (parent != null) {
        //deleteFromPreviousTree(child)
        if (child.knownTree.rootNode == child) {
          setOfTrees = setOfTrees diff Set(child.knownTree)
        }
        parent.children = parent.children :+ child
        child.knownTree = parent.knownTree
        child.parent = Some(parent)
        child.children map (changeTheAffiliatedTree(_, child.knownTree))
        Option(child.knownTree)
      }
      else {
        None
      }
    }
    else {
      None
    }
  }

  def changeTheAffiliatedTree(n : Node, t : Tree) : Unit = {
    n.knownTree = t
    n.children map (x ⇒ changeTheAffiliatedTree(x, t))
  }

  def findCommonAncestor(n1 : Node, n2 : Node) : Node = {
    if (n1 == null || n2 == null) return null
    if (n1.knownTree == n2.knownTree) n1.knownTree.rootNode else null
  }

  def deleteFromPreviousTree(child : Node, possibleRootNode : Boolean = true) : Boolean = {
    child.parent match {
      case Some(parent) ⇒
        parent.children = parent.children diff List(child) ++ child.children
      //known tree could not change as parent must belong to the same tree.
      case None ⇒
        child.children map { x ⇒
          x.parent = None
          deleteFromPreviousTree(x, false)
        }

        if (possibleRootNode) {
          setOfTrees = setOfTrees diff Set(child.knownTree)
        }
    }
    child.knownTree = null
    child.parent = null
    child.children = Nil
    true
  }
}
