package Entities

import Entities.Tsp.numberCities

import scala.util.Random

object LocalSearch {


  def twoOptFirst(tour: Vector[Integer]): Unit = {
    var pos = Vector.fill(numberCities.get)(0)
    var dlb = Vector.fill(numberCities.get)(false)
    (0 until numberCities.get).map((city) => {
      pos = pos.updated(tour(city), city)
    })
    var improvementFlag = true
    var randomVector = generateRandomPermutation()
    while (improvementFlag) {

    }
  }

  def generateRandomPermutation(): Vector[Int] = {
    Random.shuffle((0 until numberCities.get).toVector).toVector
  }
}
