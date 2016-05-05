package graph

/**
  * The weighted linker trait. Defines a linking object between two entities with some weight between them.
  * @tparam T
  */
trait WeightedLinker[T] {
  val id: Int
  val start: T
  val end: T
  val weight: Double
}

case class Edge[T](id: Int, start: T, end: T, weight: Double) extends WeightedLinker[T]

