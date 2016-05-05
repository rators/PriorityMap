package graph

import slick.driver.MySQLDriver.api._
import slick.lifted.Tag

//Defines the edges table.
class GraphTable(tag: Tag) extends Table[(Int, String, String, Double)](tag, "GRAPH") {
  // This is the primary key column
  def id = column[Int]("EDGE_ID", O.PrimaryKey)

  def start = column[String]("START_LINK")

  def end = column[String]("END_LINK")

  def weight = column[Double]("WEIGHT")

  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id, start, end, weight)
}