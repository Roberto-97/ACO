package Entities

import Entities.DistanceStrategies.DistanceStrategies

import scala.util.Random

case class TspParameters(var name: String,
                         var numberCities: Int,
                         var distanceStrategy: DistanceStrategies,
                         var nodePtr: Vector[Point],
                         var randomNumber: Random,
                         var nTours: Int,
                         var iteration: Int,
                         var lambda: Double,
                         var distance: Vector[Vector[Int]],
                         var nearestNeighborsMatrix: Array[Array[Option[Int]]] = Array.empty) extends Serializable
