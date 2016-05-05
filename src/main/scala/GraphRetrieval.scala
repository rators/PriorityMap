import graph.algorithm.DjikstraPathFinder
import graph.algorithm.kruskal.MSTKruskal
import graph.{Edge, GraphTable, LinkGraph}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import slick.driver.MySQLDriver.api._
import slick.jdbc.JdbcBackend
import slick.lifted.TableQuery
import utils.scrape.Scraping
import utils.scrape.Scraping._
import web.data.GraphResponse

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Test object for Slick and HW_3.
  */
object GraphRetrieval extends App {
  val ROOT_LINK: String = "https://en.wikipedia.org/wiki/Stan_Lee"

  // connect to my aws database.
  implicit val browser = JsoupBrowser() //start a browser for obtaining web documents.

  def run(): Unit = {
    val graph: LinkGraph = fetchGraph()
    val shortPathFinder = new DjikstraPathFinder(graph.edges)
    shortPathFinder.executeSearch("/wiki/Stan_Lee", "/wiki/Comic_books")
    val path = shortPathFinder.getPath("/wiki/Comic_books")
    println(path)
  }

  def graphResponseData(): GraphResponse = {
    val graph = fetchGraph()

    val links = graph.edges.flatMap(e => List(e.start, e.end))
      .toList
      .distinct

    GraphResponse(graph.edges.toList, links)
  }

  def fetchGraph(): LinkGraph = {
    val db = JdbcBackend.Database.forConfig("awsdb")

    val graphTable = TableQuery[GraphTable]

    val table = Await.result(db.run(graphTable.result), Duration.Inf)

    LinkGraph(MSTKruskal.getMST(toEdges(table.map(e => (e._2, e._3, e._4))).toList))
  }

  def toEdges(rowData: Iterable[(String, String, Double)]): Iterable[Edge[String]] = {
    var pop = 0
    rowData.map { data =>
      val e = Edge(pop, data._1, data._2, data._3)
      pop += 1
      e
    }
  }

  def persistGraphAction(linkGraph: LinkGraph) = linkGraph.DBActions.makeAndPopulateAction


  def getStansChildren: Iterable[Edge[String]] = {
    val stanLee = browser.get(ROOT_LINK) //Stan Lee's wiki page is the root wiki link

    println(Scraping.wikiLink(stanLee.location))

    val stanLinks = makeLinkTree(stanLee, 5) // traverse 5 levels deep, branching out 5x each at each level.

    val stanEdges = toEdges(stanLinks) // Convert the link to link and weight tuple set into edge objects.

    MSTKruskal.getMST(stanEdges.toList)
  }
}