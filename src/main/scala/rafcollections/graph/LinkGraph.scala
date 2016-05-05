package graph

import slick.driver.MySQLDriver.api._
import slick.lifted.TableQuery

/**
  * A graph that persists itself on the AWS DB.
  *
  * @param edges
  * This graph's edges.
  */
case class LinkGraph(edges: Iterable[Edge[String]]) {
  val graph = TableQuery[GraphTable]

  /**
    * The DB actions used with Slick.
    */
  object DBActions {
    def makeAndPopulateAction = DBIO.seq(createAction, populateAction)

    /**
      * The create action for this graph.
      *
      * @return
      * The create action.
      */
    def createAction = {
      val graph = TableQuery[GraphTable]
      graph.schema.create
    }

    /**
      * The populate action for this graph.
      *
      * @return
      * The populate action.
      */
    def populateAction = {
      var pop = 0
      graph ++= edges.map { edge =>
        val row = (pop, edge.start, edge.end, edge.weight)
        pop += 1
        row
      }.toSeq
    }
  }

}
