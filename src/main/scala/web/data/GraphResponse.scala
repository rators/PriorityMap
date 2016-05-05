package web.data

import graph.Edge

/**
  * A force layout graph ADT.
  */
case class GraphResponse(links: List[Edge[String]], nodes: List[String])
