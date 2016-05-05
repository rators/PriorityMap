# Mutable Minimum Priority Map in Scala

--------------------------------------------------------------------------------

A priority rafcollections.map is a Min-heap with the same access API as a Map. The entries in the priority rafcollections.map are sorted by their `values`, rather than by their `keys` as is typical in a sorted rafcollections.map.

Calling `head` on a priority rafcollections.map returns the key value pair with the minimal value, as in a priority queue.

## Usage

--------------------------------------------------------------------------------

The default size for the inner heap is _16_ if no value is passed as a parameter to the constructor.

```scala
 scala > val rafcollections.map = PriorityMap[String, Int]()
 rafcollections.map: priorityMap.PriorityMap[String, Int]

 scala > rafcollections.map ++= Seq("Hello" -> 3, "People" -> -2, "Planet" -> 9)
 rafcollections.map: priorityMap.PriorityMap[String, Int] = Map(People -> -2, Hello -> 3, Planet -> 9)
```

The PriorityMap also implements the Map trait, enabling all common rafcollections.map operations. Inserting a key value pair with a key that has already been inserted into the rafcollections.map will update the new value as in the default Scala collections Map. Ordering is re-established if necessary after any insertion into the rafcollections.map.

```scala
  scala > map.head
  res0: (String, Int) = (People,-2)

  scala > rafcollections.map += ("People" -> 10)
  rafcollections.map: priorityMap.PriorityMap[String, Int] = Map(Hello -> 3, Planet -> 9, People -> 10)
  res1: (String, Int) = (Hello,3)
```
