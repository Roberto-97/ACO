package Entities

import Entities.Tsp._

import scala.util.Random


class Ant() extends Serializable {
  private var _tour: Vector[Option[Int]] = Vector.empty
  private var _visited: Vector[Boolean] = Vector.empty
  private var _tourLength: Option[Int] = Option.empty

  /** *********************************************** Setters && Getters ****************************************************** */

  def tour = _tour

  def tour_=(tour: Vector[Option[Int]]) = {
    _tour = tour
  }

  def tourLength = _tourLength

  def tourLength_=(tourLength: Option[Int]) = {
    _tourLength = tourLength
  }

  def visited = _visited

  def visited_=(visited: Vector[Boolean]) = {
    _visited = visited
  }

  /** ************************************************************************************************************************* */


  def initializeTour(numberCities: Int): Ant = {
    _tour = Vector.fill(numberCities + 1)(Option.empty)
    this
  }

  def initializeVisited(numberCities: Int): Ant = {
    _visited = Vector.fill(numberCities + 1)(false)
    this
  }

  def initializeAnt(numberCities: Int): Ant = {
    initializeVisited(numberCities)
    initializeTour(numberCities)
    this
  }

  def randomInitialPlaceAnt(numberCities: Int, randomNumber: Random): Ant = {
    val random: Int = randomNumber.nextInt(numberCities)
    _tour = _tour.updated(0, Option(random))
    _visited = _visited.updated(random, true)
    this
  }

  def updateTour(step: Int, city: Int): Ant = {
    _tour = _tour.updated(step, Option(city))
    _visited = _visited.updated(city, true)
    this
  }

  def computeTour(numberCities: Int, distance: Vector[Vector[Int]]): Ant = {
    _tour = _tour.updated(numberCities, _tour(0))
    _tourLength = Option(computeTourLength(_tour, numberCities, distance))
    this
  }

  def clone(antToClone: Ant): Unit = {
    antToClone.tourLength = _tourLength
    antToClone._tour = _tour
    antToClone._visited = _visited
  }


}
