package Entities

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Tsp {

  private var name: String = null
  var distance: Vector[Vector[Int]] = Vector.empty
  private var optimun: Integer = null
  var numberCities: Integer = null
  private var numberNeighbors: Integer = null
  var nearestNeighborsMatrix: Vector[Vector[Option[Int]]] = Vector.empty
  private var nodeptr: Vector[Point] = Vector.empty

  def computeNearestNeighborsMatrix(): Unit = {
    var nn: Int = 0
    println("\n computing nearest neighbor lists,")
    nn = LocalSearch.depthNearestNeighbourList.toInt.max(Aco.lengthNeighborsList)
    if (nn >= numberCities)
      nn = numberCities -1
    require(numberCities > nn, "Number of cities must be mayor than depth of nearest ")

    nearestNeighborsMatrix = Vector.fill(numberCities)(Vector.fill(nn)(Option.empty))
    var node = 0

    while (node < numberCities) {
      var auxVector = (0 until numberCities).map(i => (i, distance(node)(i))).toVector
      auxVector = auxVector.updated(node, (node, Int.MaxValue))
      val nearestCities = auxVector.sortBy(_._2).map(t => Option(t._1))
      nearestNeighborsMatrix = nearestNeighborsMatrix.updated(node, (0 to nn - 1).map(i => nearestCities(i)).toVector)
      node+=1
    }
    println("\n ..done\n")
  }

  def computeTourLength(tour: Vector[Option[Integer]]): Int = {
    (0 until numberCities)
      .map((city) => distance(tour(city).get)(tour(city + 1).get))
      .reduce((distancex, distancey) => distancex + distancey)
  }

}
