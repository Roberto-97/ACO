package Entities

import Entities.Aco.{_bestSoFarAnt, computeTotalInformation, initPheromoneTrails, nnTour}
import Entities.DistanceStrategies.DistanceStrategies

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Tsp {

  private var name: String = null
  var distance: Vector[Vector[Int]] = Vector.empty
  var numberCities: Integer = null
  var nearestNeighborsMatrix: Vector[Vector[Option[Int]]] = Vector.empty
  var nodeptr: Vector[Point] = Vector.empty
  var distanceStrategy: DistanceStrategies = null
  private var _nTry: Int = 0
  var _nTours: Int = 0
  var _iteration: Int = 0
  var _restartIteration: Int = 0
  private var _restartTime: Double = 0.0
  private var _maxTries: Int = 0
  private var _maxTours: Int = 0
  private var _lambda: Double = 0.0
  private var _foundBest: Int = 0

  def initializeTspParams(name: String, numberCities: Integer, distanceStrategy: DistanceStrategies): Unit = {
    this.name = name
    this.numberCities = numberCities
    this.distanceStrategy = distanceStrategy
    this.nodeptr = Vector.fill(numberCities)(null)
  }

  def setNodeCordSection(i: Double, j: Double, pos: Int): Unit = {
    nodeptr = nodeptr.updated(pos, Point(i, j))
  }

  def heuristic(i: Int, j: Int): Double = {
    1.0 / (distance(i)(j) + 0.1)
  }

  def computeNearestNeighborsMatrix(): Unit = {
    var nn: Int = 0
    println("\n computing nearest neighbor lists,")
    nn = ExecutionParameters.nnLs.max(ExecutionParameters.nnAnts)
    if (nn >= numberCities)
      nn = numberCities -1
    require(numberCities > nn, "Number of cities must be mayor than depth of nearest ")

    nearestNeighborsMatrix = Vector.fill(numberCities)(Vector.fill(nn)(Option.empty))
    var node = 0

    while (node < numberCities) {
      var auxVector = (0 until numberCities).map(i => (i, distance(node)(i))).toVector
      auxVector = auxVector.updated(node, (node, Int.MaxValue))
      val nearestCities = auxVector
        .sortBy(_._2)
        .map(t => Option(t._1))
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

  def computeDistances(): Unit = {
    distance = Vector.fill(numberCities)(Vector.fill(numberCities)(0))
    distance = (0 until numberCities)
      .map((i) => (0 until numberCities)
      .map((j) => distanceStrategy.computeDistance(i, j)).toVector).toVector
  }

  def initTry(): Unit = {
    var trail0 = ExecutionParameters.trail0
    _nTours = 1
    _iteration = 1
    _restartIteration = 1
    _lambda = 0.05
    _bestSoFarAnt.tourLength = Int.MaxValue
    _foundBest = 0
    if (!(ExecutionParameters.acsFlag != 0  || ExecutionParameters.mmasFlag != 0 || ExecutionParameters.bwasFlag != 0)) {
      ExecutionParameters.trail0 = 1. / (ExecutionParameters.rho * nnTour())
      initPheromoneTrails(ExecutionParameters.trail0)
    }

    if (ExecutionParameters.bwasFlag != 0 || ExecutionParameters.acsFlag != 0) {
      ExecutionParameters.trail0 = 1. / (numberCities * nnTour())
      initPheromoneTrails(ExecutionParameters.trail0)
    }

    if (ExecutionParameters.mmasFlag != 0) {
      ExecutionParameters.trailMax = 1.0 / (ExecutionParameters.rho * nnTour())
      ExecutionParameters.trailMin = ExecutionParameters.trailMax / (2 * numberCities)
    }
    computeTotalInformation()
  }


}
