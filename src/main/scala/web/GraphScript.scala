package web

import utils.algorithm.DjikstraPathFinder
import graph.WeightedLinker
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.{SimpleLink, d3}
import upickle.default._
import web.data.{D3Node, GraphResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.{Failure, Success, Try}

import _root_.org.scalajs.jquery.{jQuery => $}


class WeightedLink(override val id: Int, override val sourceNode: D3Node, override val targetNode: D3Node, override val weight: Double) extends SimpleLink[D3Node](sourceNode, targetNode) with WeightedLinker[D3Node] {
  override val start = sourceNode
  override val end = targetNode
}

//todo Add two UI drop down boxes [Start] -> [End] for shortest path request seletion by the user
//todo Add a start button that sends and ajax request to the server asking for the shortest path
/**
  * Graph launcher script.
  */
object GraphScript extends js.JSApp {

  def main(): Unit = {

    getGraph.onComplete(ajaxCallback)

  }


  def getGraph: Future[dom.XMLHttpRequest] = {
    val url = "http://localhost:8080/edges"
    Ajax.get(url)
  }

  def ajaxCallback: (Try[dom.XMLHttpRequest]) => Unit = {
    case Success(response) =>
      val graph = read[GraphResponse](response.responseText)

      println(graph)

      val links = js.Array(graph.links).flatten
      val forceNodes = js.Array(graph.nodes).flatten.map(s => D3Node(s))

      val nodesMap = forceNodes.map(n => (n.linkUrl, n)).toMap

      val weightedLinks = links.map(l => new WeightedLink(l.id, nodesMap(l.start), nodesMap(l.end), l.weight))

      val width = 1920d
      val height = 1080d

      val svg = d3.select("body").append("svg")
        .attr("width", width)
        .attr("height", height)

      //Define a force object with defined properties
      //Including the dimensions of the visualization and the
      //arrays of nodes and links.
      val force = d3.layout.force()
        .charge(-200)
        .linkDistance(30)
        .size(js.Tuple2(width, height))
        .nodes(forceNodes.array)
        .links(weightedLinks)

      val link = svg.selectAll(".link")
        .data(weightedLinks)
        .enter().append("line")
        .attr("class", "link")

      val node = svg.selectAll(".node")
        .data(forceNodes)
        .enter().append("circle")
        .attr("class", "node")
        .call(force.drag)

      var startLink = "/wiki/Stan_Lee"
      var endLink = "/wiki/1963_in_comics"


      val startNode = forceNodes.find(_.linkUrl == startLink).get
      val endNode = forceNodes.find(_.linkUrl == endLink).get

      val shortPather = new DjikstraPathFinder(weightedLinks)

      shortPather.executeSearch(startNode, endNode)

      var pathNodes = shortPather.getPath(endNode).get
      var edgeLinks = shortPather.getEdgedPath(endNode).get

      def onEndForce: dom.Event => Unit = (e: dom.Event) => {

        node
          .attr("r", 5)
          .attr("cx", (d: D3Node) => {
            d.x
          })
          .attr("cy", (d: D3Node) => {
            d.y
          })


        link.attr("x1", (link: SimpleLink[D3Node]) => link.sourceNode.x)
          .attr("y1", (link: SimpleLink[D3Node]) => link.sourceNode.y)
          .attr("x2", (link: SimpleLink[D3Node]) => link.target.x)
          .attr("y2", (link: SimpleLink[D3Node]) => link.target.y)


        node.style("stroke", (d: D3Node) => if (pathNodes.contains(d)) "f00" else "ddd")

        link.style("stroke-width", (link: WeightedLink) => if (edgeLinks.contains(link)) 2d else link.weight)
          .style("stroke", (link: WeightedLink) => if (edgeLinks.contains(link)) "f00" else "ddd")
      }

      force.on("tick", onEndForce)

      force.start()

      $(".startUrl").each((ind: Int, elem: dom.Element) => {
        $(elem).click {
          () => {
            startLink = elem.innerHTML
          }
        }
      })

      $(".endUrl").each((ind: Int, elem: dom.Element) => {
        $(elem).click {
          () => {
            endLink = elem.innerHTML
          }
        }
      })

      $("#confirmButton").click {
        () => {
          val startNode = forceNodes.find(_.linkUrl == startLink).get
          val endNode = forceNodes.find(_.linkUrl == endLink).get

          val shortPather = new DjikstraPathFinder(weightedLinks)

          shortPather.executeSearch(startNode, endNode)

          println("The start node is" + startNode)
          println("The end node is" + endNode)

          val checkPath = shortPather.getPath(endNode)

          checkPath match {
            case None => println("No path available!")
            case Some(v) => {
              force.stop()
              println(shortPather.getEdgedPath(endNode).get)
              println("The start node is" + startNode)
              println("The end node is" + endNode)
              pathNodes = shortPather.getPath(endNode).get
              edgeLinks = shortPather.getEdgedPath(endNode).get
              force.start()
            }
          }


        }
      }


    case Failure(f) => throw f
  }

}
