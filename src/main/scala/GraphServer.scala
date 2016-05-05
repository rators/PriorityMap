import jsonprotocols.GraphDataJsonProtocols._
import akka.actor.ActorSystem
import spray.http.HttpEntity
import spray.http.MediaTypes.{`text/html` => htmlText}
import spray.json.DefaultJsonProtocol
import spray.routing.SimpleRoutingApp
import view.Index

import scalatags.Text.{Attrs => _, Styles => _, Tags => _}

/**
  * The spray server application.
  */
object GraphServer extends App with SimpleRoutingApp with DefaultJsonProtocol {
  implicit val actorSystem = ActorSystem()

  startServer(interface = "localhost", port = 8080) {
    get {
      path("rafcollections/graph") {
        complete {
          val allUrls = GraphRetrieval.graphResponseData().nodes
          println(allUrls.size)
          HttpEntity(htmlText, Index.HTML(allUrls))
        }
      } ~ pathPrefix("") {
        compressResponse() {
          getFromResourceDirectory("web")
        }
      }
    } ~
      get {
        path("edges") {
          complete {
            GraphRetrieval.graphResponseData()
          }
        }
      }
  }
}
