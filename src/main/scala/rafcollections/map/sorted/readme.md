# Mutable Minimum Priority Map in Scala

--------------------------------------------------------------------------------

A priority map is a Min-heap with the same access API as a Map. The entries in the priority map are sorted by their `values`, rather than by their `keys` as is typical in a sorted map.

Calling `head` on a priority map returns the key value pair with the minimal value, as in a priority queue.

## Usage

--------------------------------------------------------------------------------

The default size for the inner heap is _16_ if no value is passed as a parameter to the constructor.

```scala
 scala > val map = PriorityMap[String, Int]()
 map: priorityMap.PriorityMap[String, Int]

 scala > map ++= Seq("Hello" -> 3, "People" -> -2, "Planet" -> 9)
 map: priorityMap.PriorityMap[String, Int] = Map(Hello -> 3, People -> -2, Planet -> 9)
```

The PriorityMap also implements the Map trait, enabling all common map operations. Inserting a key value pair with a key that has already been inserted into the map will update the new value as in the default Scala collections Map. Ordering is re-established if necessary after any insertion into the map.

```scala
  scala > map.head
  res0: (String, Int) = (People,-2)

  scala > map += ("People" -> 10)
  map: priorityMap.PriorityMap[String, Int] = Map(Hello -> 3, People -> 10, Planet -> 9)
  res1: (String, Int) = (Hello,3)
```
