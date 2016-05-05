package rafcollections.map

import scala.annotation.tailrec
import scala.collection.mutable

case class PMapNode[K, V](key: K, var value: V)(implicit ev: V => Ordered[V]) extends Ordered[PMapNode[K, V]] {
  override def compare(that: PMapNode[K, V]): Int = this.value compare that.value
}

/**
  * A priority queue backed by a map.
  *
  * @param initSize
  * The initial size of the inner heap used to hold the tree structure.
  * @param ev
  * An implicit conversion for type V to an ordered type.
  * @tparam K
  * The type of the key.
  * @tparam V
  * The type of the value.
  */
class PriorityMap[K, V](initSize: Int = 16)(implicit ev: V => Ordered[V]) extends mutable.Map[K, V] {
  type MapNode = PMapNode[K, V]

  val ROOT_INDEX = 1

  // The root of the heap is always in index 1
  private var heap = new Array[MapNode](initSize) //

  private var _size = 0

  private val reloadFactor = 0.75d

  private var indexMap: mutable.HashMap[K, Int] = mutable.HashMap()

  override def size = _size

  override def +=(kv: (K, V)): PriorityMap.this.type = {
    insert(kv)
    this
  }

  override def -=(key: K): PriorityMap.this.type = {
    pop(key)
    this
  }

  override def get(key: K): Option[V] = {
    indexMap.get(key) match {
      case None => None
      case Some(ind) => Some(heap(ind).value)
    }
  }

  override def iterator: Iterator[(K, V)] = {
    indexMap.values
      .map { i =>
        (heap(i).key, heap(i).value)
      }.toList.sortWith(_._2 < _._2).iterator
  }

  /**
    * The highest ordered element in the heap.
    *
    * @return
    * The element that is highest ordered.
    */
  override def head = heap(ROOT_INDEX)

  /**
    * Removes the highest ordered element in the heap from this collection and returns that element.
    *
    * @return
    * The highest ordered element in the heap.
    */
  def pop: Option[(K, V)] = pop(heap(ROOT_INDEX).key)

  private def shouldReload: Boolean = {
    val currentFillRate: Double = _size.toDouble / heap.length.toDouble
    currentFillRate >= reloadFactor
  }

  private def reload(): Unit = {
    heap ++= new Array[MapNode](heap.length)
  }

  private def tryReload() = if (shouldReload) reload()

  private def appendIndex = _size + 1

  private def getChildren(index: Int): ((Option[MapNode], Int), (Option[MapNode], Int)) = {
    val leftInd = index * 2
    val rightInd = leftInd + 1

    (index * 2 <= _size, index * 2 + 1 <= _size) match {
      case (hasLeft, hasRight) =>
        val leftChild = hasLeft match {
          case true => Some(heap(leftInd))
          case false => None
        }
        val rightChild = hasRight match {
          case true => Some(heap(rightInd))
          case false => None
        }

        ((leftChild, leftInd), (rightChild, rightInd))
    }

  }

  private def parentIndex(index: Int) = index / 2

  private def insert(kv: (K, V)): Unit = {
    tryReload()
    indexMap.contains(kv._1) match {
      case true =>
        update(indexMap(kv._1), kv._2)
      case false =>
        val insertIndex = appendIndex
        heap(insertIndex) = PMapNode[K, V](kv._1, kv._2)
        indexMap += (kv._1 -> insertIndex)
        _size += 1
        siftUp(insertIndex)
    }
  }

  private def update(ind: Int, value: V): Unit = {
    val target = heap(ind)

    target.value = value

    val parentInd = parentIndex(ind)

    parentInd match {
      case 0 => siftDown(ind)
      case _ => heap(parentInd) match {
        case parentNode if parentNode.value > value => siftUp(ind)
        // Do nothing with the parent. Check to sift up next.
        case _ => siftDown(ind)
      }
    }
  }

  @tailrec
  private def siftUp(targetInd: Int): Unit = {
    parentIndex(targetInd) match {
      case parentInd if parentInd != 0 =>
        val parentNode = heap(parentInd)
        val currentNode = heap(targetInd)

        currentNode < parentNode match {
          case true =>
            swap(targetInd, (parentNode, parentInd))
            siftUp(parentInd)
          case false => ()
        }
      case _ => ()
    }
  }

  /*
    * Swaps two nodes.
    * @param aInd
    *             The index of the first node.
    * @param b
    *          The second node and it's position in the heap.
    */
  private def swap(aInd: Int, b: (MapNode, Int)): Unit = {
    val temp = heap(aInd)

    heap(aInd) = b._1
    heap(b._2) = temp

    indexMap += (temp.key -> b._2)
    indexMap += (b._1.key -> aInd)
  }

  @tailrec
  private def siftDown(targetInd: Int): Unit = {
    getChildren(targetInd) match {
      case ((None, _), (right: Some[MapNode], rightInd)) => // Right child only
        right.get match {
          case rightChild =>
            rightChild < heap(targetInd) match {
              case true =>
                swap(targetInd, (rightChild, rightInd))
                siftDown(rightInd)
              case _ => // Done sifting down, the right child is the only child and it is greater than the current node, nothing can be done.
            }
        }
      case ((left: Some[MapNode], leftInd), (None, _)) => // Left child only
        left.get match {
          case leftChild =>
            leftChild < heap(targetInd) match {
              case true =>
                swap(targetInd, (leftChild, leftInd))
                siftDown(leftInd)
              case _ => // Done sifting down, do nothing here.
            }
        }
      case ((left: Some[MapNode], leftInd), (right: Some[MapNode], rightInd)) => {
        val childL = left.get
        val childR = right.get
        // left and right child
        val smallestChild: (MapNode, Int) = if (childL < childR) (left.get, leftInd) else (right.get, rightInd) // choose smallest child
        smallestChild._1 < heap(targetInd) match {
          case true =>
            swap(targetInd, smallestChild)
            siftDown(smallestChild._2)
          case _ => // Done sifting down, the left child is the only child and it is greater than the current node, nothing can be done.
        }
      }
      case (((None, _), (None, _))) => // No children, node has sifted to the end of tree, done sifting down.
    }
  }

  private def pop(key: K): Option[(K, V)] = {
    _size match {
      case 0 => None // If the size is 0 then do nothing and return none.
      case 1 => // If there is only one item then send it up and clear all state.
        _size = 0

        val node = heap(1)
        indexMap.clear()
        Some(node)
      case rightMostInd => // Decrease size and replace the parent node with the right most node in the heap.
        _size -= 1

        val rightMostNode = heap(_size)
        val minNode = heap(1)

        heap(1) = rightMostNode
        // Update the location in indexMap for the node swapped and start shifting it down.
        indexMap -= key
        indexMap += (rightMostNode.key -> 1)

        siftDown(1)
        Some(minNode)
    }
  }

  private implicit def toTuple(node: MapNode): (K, V) = (node.key, node.value)

}

object PriorityMap {
  def apply[K, V](initSize: Int = 16)(implicit ev: V => Ordered[V]): PriorityMap[K, V] = new PriorityMap[K, V]()

  def apply[K, V](traversableOnce: TraversableOnce[(K, V)])(implicit ev: V => Ordered[V]): PriorityMap[K, V] =
    PriorityMap[K, V](traversableOnce.size) ++= traversableOnce
}

object Example extends App {
  val map = PriorityMap(Seq("Hello" -> 3, "People" -> -2, "Planet" -> 9))
  println(map)
  println(map.head)
  map += ("People" -> 10)
  println(map)
  println(map.head)
}