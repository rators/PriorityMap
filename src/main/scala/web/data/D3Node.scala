package web.data

import org.singlespaced.d3js.forceModule.Node
import upickle.Js
import upickle.Js.Str

case class D3Node(linkUrl: String, nx: Double = 0d, ny: Double = 0d) extends Node {
  x = nx
  y = ny
}

object D3NodePickler {

  implicit val d3NodeWrite = upickle.default.Writer[D3Node] {
    case d => Js.Obj(("linkUrl", Str(d.linkUrl)))
  }

  implicit val d3Reader = upickle.default.Reader[D3Node] {
    case Js.Obj(("linkUrl", nname: Str)) =>
      new D3Node(nname.value)
  }

}