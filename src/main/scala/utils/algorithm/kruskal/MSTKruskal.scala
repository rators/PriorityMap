package graph.algorithm.kruskal

import graph.Edge

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Creates a minimum spanning tree from a weighted edge graph.
  */
object MSTKruskal {
  def getMST(edges: List[Edge[String]]): List[Edge[String]] = {
    val mst = ListBuffer[Edge[String]]()

    val orderedEdges = mutable.PriorityQueue.empty(EdgeOrder) ++= edges
    val unionFind = UnionFind(edges)

    orderedEdges.foreach { edge =>
      unionFind.connected(edge.start, edge.end) match {
        case true => ()
        case false =>
          mst += edge
          unionFind.union(edge.start, edge.end)
      }
    }

    mst.toList
  }

  object EdgeOrder extends Ordering[Edge[String]] {
    override def compare(x: Edge[String], y: Edge[String]): Int = y.weight compare x.weight
  }

}
