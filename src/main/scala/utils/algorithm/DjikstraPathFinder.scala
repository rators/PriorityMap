package utils.algorithm

import graph.WeightedLinker

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import rafcollections.map.PriorityMap

/**
  * Dijkstra shortest path.
  */
class DjikstraPathFinder[T](edges: Iterable[WeightedLinker[T]]) {
  var settledNodes: collection.mutable.Set[T] = mutable.Set.empty

  val predecessors: collection.mutable.Map[T, T] = collection.mutable.Map.empty

  // The distance tags associated with a node.
  var unSettledNodes = PriorityMap[T, Double]()

  def clear() {
    settledNodes.clear()
    unSettledNodes = PriorityMap[T, Double]()
    predecessors.clear()
  }

  /*
    * Intuitively the O(E*logV) makes sense, because worst case you'll have to go through every
    * possible connection and at each time you may need to push or pop a value into the queue to decide where to
    * go next.
    *
    */
  def executeSearch(source: T, destination: T) {
    clear()

    unSettledNodes += (source -> 0) // Add the source node to the unsettled nodes list.

    while (unSettledNodes.nonEmpty && !settledNodes.contains(destination)) {
      // All nodes may end up in unsettled worst case this is the O(V ...
      /* As long as there are unsettled nodes and the destination
       * has not been added to the settled nodes (path has been found) continue.
       */
      val node = unSettledNodes.pop.get._1
      // Pop ^ requires Log(N) for the re-order worst case so this while loop so far up to here is for
      //for nodes V do a pop from a priority queue so O(V*logV + ...

      settledNodes.add(node)

      // This could easily be pulled out and iterated over for every edge that remains that contains a neighbor
      // of the current node, this number is independent of the node over time because all edges will need to be checked regardless.

      expandAndFindMinimalDist(node)

      /*
       * This tag is added because as you exhaust nodes you also exhaust edges
       * So this function is not dependent on the iteration but rather on the number of remaining edges that need to be checked
       * at this iteration which is not dependent on the fact that this nodes is node n in the list of edges.
       *
       */
    }

  }

  /**
    * Iterate through all the neighbors of a node.
    * O(E*logV)
    *
    * @param node
    * The node whose neighbors are being given tentative distances.
    */
  def expandAndFindMinimalDist(node: T) {
    val adjacentNodes = getNeighbors(node)
    for (target <- adjacentNodes) {
      /*
       * Iterate through all node neighbors requires getting all the edges for a node, if we have to do this for all
       * nodes at worst case then so far here we have O(E
       */

      /*
       * If the current tentative distance for TARGET is greater than the tentative distance required to go from CURR_NODE -> TARGET
       * then update the current tentative distance for B to the new smaller tentative value (T -> B) WEIGHT.
       *
       * Add the predecessor path TARGET TO NODE so we know to go from the current NODE to the newly updated TARGET node, since this path is shorter.
       * Add the TARGET node back to the unsettled nodes heap with the new distance.
       */
      if (getShortestDistance(target) > getShortestDistance(node) + getDistance(node, target)) {
        /*
         * All nodes may be pushed for every edge if the new distance found is always better than the current tentative value.
         * Now we have O(E*logV).
        */
        predecessors += (target -> node)
        unSettledNodes += (target -> (getShortestDistance(node) + getDistance(node, target)))
      }
    }
  }

  /*
    * Get the tentative distance from the source to the @param destination to the source node.
    *
    * @param destination
    * The destination node.
    * @return
    * The tentative distance from source to @param destination.
    */
  def getShortestDistance(destination: T): Double = unSettledNodes.getOrElse(destination, Double.MaxValue)


  /*
    * Gets the distance from one node to another.
    *
    * @param node
    * The from node.
    * @param target
    * The destination node.
    * @return
    * The distance to reach the destination node from the source.
    */
  private def getDistance(node: T, target: T): Double = {
    edges.find(e => e.start == node && e.end == target) match {
      case Some(edge) => edge.weight
      case _ => throw new RuntimeException(s"Node edge from $node to $target")
    }
  }

  /*
    *
    * @param node
    * The node that may have neighbor nodes.
    * @return
    *     An iterable containing this nodes neighbors, if any.
    */
  def getNeighbors(node: T): Iterable[T] = {
    edges.filter { e =>
      e.start == node && !settledNodes.contains(e.end)
    }.map(_.end)
  }

  /*
    * Gets the nodes with the shortest tentative distance from the source node.
    *
    * @return
    * The node with the shortest tentative distance from the source node.
    */
  def getMinimum: T = unSettledNodes.head._1

  /**
    * Gets the path as a sequence of links.
    *
    * @param target
    * The target node.
    * @return
    * A path if any exists to this node.
    */
  def getPath(target: T): Option[ListBuffer[T]] = {
    val path = ListBuffer[T]()
    var step = target

    if (predecessors.get(step).isEmpty) {
      return None
    }

    path += step

    while (predecessors.get(step).isDefined) {
      step = predecessors(step)
      path += step
    }

    Some(path.reverse)
  }

  /**
    * Gets the path as edges.
    *
    * @param target
    * The target node.
    * @return
    * An optional of the shortest path.
    */
  def getEdgedPath(target: T): Option[ListBuffer[WeightedLinker[T]]] = {
    val pathPairs = getPath(target)
    pathPairs match {
      case None => None
      case Some(pairs) => Some((pairs zip pairs.tail).map { tup =>
        edges.find(edge => edge.start == tup._1 && edge.end == tup._2).get
      })
    }
  }


}