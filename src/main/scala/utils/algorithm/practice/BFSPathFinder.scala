package utils.algorithm.practice

import scala.collection.mutable

case class Vertex[T](value: T, var distance: Int = Int.MaxValue, var parent: Option[Vertex[T]] = None)

case class UnweightedEdge[T](start: Vertex[T], end: Vertex[T])

case class Graph[T](edges: Iterable[UnweightedEdge[T]])

/**
  * Find the shortest path from a source node to some other node using BFS.
  *
  * Worst Case : E + V: May have to traverse a linked list like structure from end to end.
  */
object BFSPathFinder extends App {
  val NURNBERG: String = "Nurnberg"
  val STUTTGART: String = "Stuttgart"
  val FRANKFURT: String = "Frankfurt"
  val MANHEIM: String = "Manheim"
  val KARLS: String = "Karlsruhe"
  val AUGS: String = "Augsburg"
  val MUNCH: String = "Munchen"
  val WUZBERG: String = "Wurzburg"
  val ERFURT: String = "Erfurt"
  val KASSEL: String = "Kassel"

  val places = Iterable(
    FRANKFURT, MANHEIM, KARLS, AUGS, MUNCH,
    KASSEL, STUTTGART, NURNBERG, WUZBERG, ERFURT
  )

  val vertices = places.map(s => (s, Vertex[String](s))).toMap

  val germanEdges = Iterable[UnweightedEdge[String]](
    UnweightedEdge(vertices(FRANKFURT), vertices(MANHEIM)),
    UnweightedEdge(vertices(MANHEIM), vertices(KARLS)),
    UnweightedEdge(vertices(KARLS), vertices(AUGS)),
    UnweightedEdge(vertices(AUGS), vertices(MUNCH)),

    UnweightedEdge(vertices(FRANKFURT), vertices(KASSEL)),
    UnweightedEdge(vertices(KASSEL), vertices(MUNCH)),

    UnweightedEdge(vertices(FRANKFURT), vertices(WUZBERG)),
    UnweightedEdge(vertices(WUZBERG), vertices(ERFURT)),
    UnweightedEdge(vertices(WUZBERG), vertices(NURNBERG)),
    UnweightedEdge(vertices(NURNBERG), vertices(MUNCH)),

    UnweightedEdge(vertices(NURNBERG), vertices(STUTTGART))
  )

  val shortPath = shortestPath(vertices(FRANKFURT), vertices(FRANKFURT), vertices(STUTTGART), germanEdges)

  if (shortPath.isDefined) println(shortPath.get.map(_.value)) else println(shortPath)

  def shortestPath[T](root: Vertex[T], start: Vertex[T], end: Vertex[T], edges: Iterable[UnweightedEdge[T]]): Option[Iterable[Vertex[T]]] = {
    val queue = mutable.Queue[Vertex[T]]()

    root.distance = 0
    queue.enqueue(root)

    while (queue.nonEmpty) {
      val currentVertex = queue.dequeue()

      for (neighbor <- getNeighbors(currentVertex, edges)) {
        if (neighbor.distance == Int.MaxValue) {
          // Prevent cycles
          neighbor.distance = currentVertex.distance + 1
          neighbor.parent = Some(currentVertex)
          queue.enqueue(neighbor)
        }
      }
    }


    getPath(start, end, edges)
  }

  def getPath[T](start: Vertex[T], goal: Vertex[T], taggedEdges: Iterable[UnweightedEdge[T]]): Option[Iterable[Vertex[T]]] = {
    val queue = mutable.ArrayStack[Vertex[T]]()

    queue.push(start)

    while (queue.top.parent.isDefined && queue.top != goal) {
      val current = queue.top
      if (current.parent.isDefined) queue.push(current.parent.get)
    }

    if (queue.top == goal) Some(queue) else None
  }

  def getNeighbors[T](start: Vertex[T], edges: Iterable[UnweightedEdge[T]]): Iterable[Vertex[T]] = {
    val toEdges = edges.filter(e => e.start == start)

    toEdges.map(_.end)
  }
}