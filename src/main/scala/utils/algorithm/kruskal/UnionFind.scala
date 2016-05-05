package graph.algorithm.kruskal

import graph.Edge

import scala.annotation.tailrec
import scala.collection.mutable

case class Node(parent: Option[String], treeSize: Int)


object MinOrder extends Ordering[String] {

  override def compare(x: String, y: String) = y compareTo x

}

object UnionFind {

  def apply(nodes: Vector[Node], allLinks: Map[String, Int]): UnionFind = new UnionFind(nodes, allLinks)

  def apply(edges: Iterable[Edge[String]]): UnionFind = {
    val indexMap = edgeIndexMap(edges)
    val size = indexMap.size
    val nodes = Vector.fill(size)(Node(None, 1))

    UnionFind(nodes, indexMap)
  }

  implicit def edgeIndexMap(edges: Iterable[Edge[String]]): Map[String, Int] = {
    val minHeap: mutable.PriorityQueue[String] = mutable.PriorityQueue.empty(MinOrder)
    val allLinks: List[String] = edges
      .flatMap { edge => List(edge.start, edge.end) }
      .toList.distinct

    val sortedList = (minHeap ++ allLinks).toList

    sortedList
      .map { link =>
        (link, sortedList.indexOf(link))
      }.toMap
  }
}

/**
  * Purely functional union find implementation.
  */
class UnionFind(nodes: Vector[Node], allLinks: Map[String, Int]) {

  def union(fromNode: String, toNode: String): UnionFind = {
    // if x == y then x and y are in the same set then
    // do nothing an return this set. Connected returns true
    // before utilizing the data structure to check connectivity.
    if (fromNode == toNode) return this

    val rootA = representative(fromNode)
    val rootB = representative(toNode)

    //If x and y are already in the same set do nothing and return
    //this instance of UF.n
    if (rootA == rootB) return this

    val nodeA = nodes(rootA)
    val nodeB = nodes(rootB)

    val newTreeSize = nodeA.treeSize + nodeB.treeSize

    //Append the smaller set (sS) to the larger set (lS) by making sS representative
    //the representative of the lS. Update the tree size of the sets.
    val (newNodeA, newNodeB) = nodeA.treeSize < nodeB.treeSize match {
      case true =>
        val newNodeA = Node(Some(rootB), newTreeSize)
        val newNodeB = Node(nodeB.parent, newTreeSize)
        (newNodeA, newNodeB)
      case false =>
        val newNodeA = Node(Some(rootA), newTreeSize)
        val newNodeB = Node(nodeA.parent, newTreeSize)
        (newNodeA, newNodeB)
    }

    val newNodes = nodes.updated(rootA, newNodeA).updated(rootB, newNodeB)
    new UnionFind(newNodes, allLinks)
  }

  def connected(fromLink: String, toLink: String): Boolean = fromLink == toLink || representative(fromLink) == representative(toLink)

  @tailrec
  private def representative(link: String): String = nodes(link).parent match {
    case None => link
    case Some(parent) => representative(parent)
  }

  implicit def targetIndex(link: String): Int = allLinks(link)
}
