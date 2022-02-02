package Entities

import scala.jdk.CollectionConverters.*

object Tsp {

  private val distance: Vector[Vector[Int]] = Vector.empty

  def computeNnList(nnLocalSearch: Int, nnAnts: Int, numberCities: Int): Vector[Vector[Int]] = {
    var nn: Int = 0
    var distanceVector: Vector[Int] = Vector.fill(numberCities)(0)
    var helpVector: Vector[Int] = Vector.fill(numberCities)(0)
    var mnNear: Vector[Vector[Int]] = Vector.empty
    var mapCitiesNearest: Map[Int, Int] = Map.empty
    println("\n computing nearest neighbor lists,")
    nn = nnLocalSearch.max(nnAnts)
    if (nn >= numberCities)
      nn = numberCities -1
    require(numberCities > nn, "Number of cities must be mayor than depth of nearest ")

    mnNear = Vector.fill(nn)(Vector.fill(nn)(0))
    var node = 0

    while (node < numberCities) {
      (0 until numberCities).map(i => {
        distanceVector = distanceVector.updated(i,distance(node)(i))
        helpVector = helpVector.updated(i, i)
        mapCitiesNearest += distance(node)(i) -> i
      })
      distanceVector = distanceVector.updated(node, Int.MaxValue)
      distanceVector = distanceVector.sorted
      helpVector = distanceVector.map(key => mapCitiesNearest.get(key).get)
      mnNear = mnNear.updated(node, (0 until nn).map(i => helpVector(i)).toVector)
      node+=1
    }
    println("\n ..done\n")
    mnNear
  }

}
