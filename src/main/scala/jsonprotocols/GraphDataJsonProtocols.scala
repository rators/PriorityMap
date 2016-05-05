package jsonprotocols

import graph.Edge
import spray.httpx.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsObject, JsonFormat, RootJsonFormat}
import web.data.GraphResponse
import spray.json._

object GraphDataJsonProtocols extends DefaultJsonProtocol with SprayJsonSupport {

  implicit def edgeFormat[A : JsonFormat] = jsonFormat4(Edge[A])

  implicit object GraphResponseFormat extends RootJsonFormat[GraphResponse] {
    def write(gr: GraphResponse) = JsObject(
      "links" -> gr.links.toJson,
      "nodes" -> gr.nodes.toJson
    )
    def read(value: JsValue) = {
      value.asJsObject.getFields("links", "nodes") match {
        case Seq(links: JsArray, nodes: JsArray) =>
          val nodeArray = nodes.elements.map(_.toString()).toList
          val edgeArray = links.elements.map{ value =>
            value.asJsObject.getFields("id", "start", "end", "weight") match {
              case Seq(id: JsNumber, start: JsString, end: JsString, weight: JsNumber) =>
                Edge(id.value.toInt, start.toString(), end.toString(), weight.value.toDouble)
            }
          }.toList
          new GraphResponse(edgeArray, nodeArray)
        case _ => throw new DeserializationException("Unexpected error when reading Graph Response")
      }
    }
  }
}