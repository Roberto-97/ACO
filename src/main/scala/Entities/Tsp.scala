package Entities

import Entities.Aco.*
import Entities.DistanceStrategies.DistanceStrategies
import Entities.ExecutionParameters.*

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Random

object Tsp {

  private var _name: String = null
  private var _distance: Vector[Vector[Int]] = Vector.empty
  private var _numberCities: Integer = null
  private var _nearestNeighborsMatrix: Array[Array[Option[Int]]] = Array.empty
  private var _nodeptr: Vector[Point] = Vector.empty
  private var _distanceStrategy: DistanceStrategies = null
  private var _randomNumber: Random = null

  /************************************************* Setters && Getters *******************************************************/

  def name = _name
  def name_=(name:String) = {
    _name = name
  }

  def distance = _distance
  def distance_=(distance:Vector[Vector[Int]]) = {
    _distance = distance
  }

  def numberCities = _numberCities
  def numberCities_=(numberCities:Integer) = {
    _numberCities = numberCities
  }

  def nearestNeighborsMatrix = _nearestNeighborsMatrix
  def nearestNeighborsMatrix_=(nearestNeighborsMatrix:Array[Array[Option[Int]]]) = {
    _nearestNeighborsMatrix = nearestNeighborsMatrix
  }

  def nodeptr = _nodeptr
  def nodeptr_=(nodeptr:Vector[Point]) = {
    _nodeptr = nodeptr
  }

  def distanceStrategy = _distanceStrategy
  def distanceStrategy_=(distanceStrategy:DistanceStrategies) = {
    _distanceStrategy = distanceStrategy
  }

  def randomNumber = _randomNumber
  def randomNumber_=(randomNumber: Random) = {
    _randomNumber = randomNumber
  }

  /****************************************************************************************************************************/

  def initializeTspParams(name: String, numberCities: Integer, distanceStrategy: DistanceStrategies): Unit = {
    this._name = name
    this._numberCities = numberCities
    this._distanceStrategy = distanceStrategy
    this._nodeptr = Vector.fill(numberCities)(null)
    this._randomNumber = new Random(seed)
  }

  def setNodeCordSection(i: Double, j: Double, pos: Int): Unit = {
    _nodeptr = _nodeptr.updated(pos, Point(i, j))
  }

  def heuristic(i: Int, j: Int): Double = {
    1.0 / (_distance(i)(j) + 0.1)
  }

  def computeNearestNeighborsMatrix(): Unit = {
    var nn: Int = 0
    println("Computing nearest neighbor lists ..")
    nn = nnLs.max(nnAnts)
    if (nn >= _numberCities)
      nn = _numberCities -1
    require(_numberCities > nn, "Number of cities must be mayor than depth of nearest ")

    _nearestNeighborsMatrix = Array.fill(_numberCities)(Array.fill(nn)(Option.empty))
    var node = 0
    while (node < _numberCities) {
      var auxVector = (0 until _numberCities).map(i => (i, _distance(node)(i))).toVector
      auxVector = auxVector.updated(node, (node, Int.MaxValue))
      val nearestCities = auxVector
        .sortBy(_._2)
        .map(t => Option(t._1))
      for (i <- 0 until nn) {
        _nearestNeighborsMatrix(node)(i) = nearestCities(i)
      }
      node+=1
    }
    println("done ..")
  }

  def computeTourLength(tour: Vector[Option[Integer]]): Int = {
    (0 until _numberCities)
      .map((city) => _distance(tour(city).get)(tour(city + 1).get))
      .reduce((distancex, distancey) => distancex + distancey)
  }

  def computeDistances(): Unit = {
    _distance = Vector.fill(_numberCities)(Vector.fill(_numberCities)(0))
    _distance = (0 until _numberCities)
      .map((i) => (0 until _numberCities)
      .map((j) => _distanceStrategy.computeDistance(i, j)).toVector).toVector
  }

}
