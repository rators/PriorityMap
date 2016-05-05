package graph.algorithm

import rafcollections.map.HashMap

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Measures the Cosine similarity of two vectors of an inner product space and
  * compares the angle between them.
  *
  * <p>
  * For further explanation about the Cosine Similarity, refer to
  * http://en.wikipedia.org/wiki/Cosine_similarity.
  * </p>
  */
object CosineSimilarity {
  /**
    * Calculates the cosine similarity for two given vectors.
    *
    * @param leftVector  left vector
    * @param rightVector right vector
    *                    @ cosine similarity between the two vectors
    */
  def cosineSimilarity(leftVector: HashMap[CharSequence, Int], rightVector: HashMap[CharSequence, Int]): Double = {
    if (leftVector == null || rightVector == null) {
      throw new IllegalArgumentException("Vectors must not be null")
    }
    val intersection = getIntersection(leftVector, rightVector)
    val dotProduct: Double = dot(leftVector, rightVector, intersection)
    var d1: Double = 0.0d
    for (value <- leftVector.values) {
      d1 += Math.pow(value.doubleValue(), 2)
    }
    var d2: Double = 0.0d
    for (value <- rightVector.values) {
      d2 += Math.pow(value.doubleValue(), 2)
    }
    var cosineSimilarity: Double = .0
    if (d1 <= 0.0 || d2 <= 0.0) {
      cosineSimilarity = 0.0
    }
    else {
      cosineSimilarity = dotProduct / (Math.sqrt(d1) * Math.sqrt(d2))
    }
    cosineSimilarity
  }

  /**
    * s a set with strings common to the two given maps.
    *
    * @param leftVector  left vector rafcollections.map
    * @param rightVector right vector rafcollections.map
    *                    @ common strings
    */
  private def getIntersection(leftVector: HashMap[CharSequence, Int], rightVector: HashMap[CharSequence, Int]) = {
    val intersection = mutable.HashSet[CharSequence]()
    leftVector.keySet.foreach(intersection += _)
    intersection.retainAll(rightVector.keySet)
    intersection
  }

  /**
    * Computes the dot product of two vectors. It ignores remaining elements. It means
    * that if a vector is longer than other, then a smaller part of it will be used to compute
    * the dot product.
    *
    * @param leftVector   left vector
    * @param rightVector  right vector
    * @param intersection common elements
    *                     @ the dot product
    */
  private def dot(leftVector: HashMap[CharSequence, Int], rightVector: HashMap[CharSequence, Int], intersection: java.util.Set[CharSequence]): Double = {
    var dotProduct: Long = 0
    for (key <- intersection) {
      dotProduct += leftVector.get(key).get * rightVector.get(key).get
    }
    dotProduct
  }
}
