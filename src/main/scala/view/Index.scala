package view

import scalatags.Text.TypedTag
import scalatags.Text.all._

/**
  * The main view.
  */
object Index {

  val DROP_DOWN_START = "dropDownStart"
  val DROP_DOWN_END = "dropDownEnd"

  private def homeFrag(linkUrls: Seq[String]): TypedTag[String] =
    html(
      head(
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        link(rel := "stylesheet", href := "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.6/css/materialize.min.css"),
        script(`type` := "text/javascript", src := "http://cdn.jsdelivr.net/jquery/2.1.1/jquery.js"),
        script(`type` := "text/javascript", src := "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.6/js/materialize.min.js"),
        script(src := "http://d3js.org/d3.v3.min.js"),
        script(src := "graphhw3-fastopt.js"),
        title := "Stan Lee's Graph"
      ),
      link(rel := "stylesheet", `type` := "text/css", href := "css/graphstyle.css"),
      body(
        div(`class` := "row container")(
          dropDownButton(DROP_DOWN_START)("Start Link"),
          dropDownButton(DROP_DOWN_END)("End Link")
        ),
        a(`class` := "waves-effect waves-light btn", id := "confirmButton")("button")
      ),
      table(linkUrls, "startUrl", DROP_DOWN_START),
      table(linkUrls, "endUrl", DROP_DOWN_END),
      script("web.GraphScript().main()")
    )


  def HTML(linkUrls: Seq[String]) = homeFrag(linkUrls).render

  private def table(linkUrls: Seq[String], linkClass: String, dropDownId: String) =
    ul(id := dropDownId, `class` := "dropdown-content")(
      for (link <- linkUrls.sortWith(_ < _)) yield {
        li(a(href := "#!", `class` := linkClass) {
          link
        })
      }
    )

  def dropDownButton(dataActivateId: String) = a(`class` := "dropdown-button btn col s8 m8 l8", href := "#", "data-activates".attr := dataActivateId)

}
