package Entities

import Entities.Tsp.numberCities

import scala.util.Random
object LocalSearch {


  def twoOptFirst(tour: Vector[Integer]): Unit = {
    var pos = Vector.fill(numberCities)(0)
    var dlb = Vector.fill(numberCities)(false)
    (0 until numberCities).map((city) => {
      pos = pos.updated(tour(city), city)
    })
    var improvementFlag = true
    var randomVector = generateRandomPermutation()
    while (improvementFlag) {

    }
  }

  def generateRandomPermutation(): Vector[Int] = {
    Random.shuffle((0 until numberCities).toVector).toVector
  }
}
