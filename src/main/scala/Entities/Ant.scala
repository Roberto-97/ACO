package Entities

import Entities.Tsp._


class Ant() extends Serializable {
  private var _tour: Vector[Option[Integer]] = Vector.empty
  private var _visited: Vector[Boolean] = Vector.empty
  private var _tourLength: Integer = null

  /** *********************************************** Setters && Getters ****************************************************** */

  def tour = _tour

  def tour_=(tour: Vector[Option[Integer]]) = {
    _tour = tour
  }

  def tourLength = _tourLength

  def tourLength_=(tourLength: Integer) = {
    _tourLength = tourLength
  }

  def visited = _visited

  def visited_=(visited: Vector[Boolean]) = {
    _visited = visited
  }

  /** ************************************************************************************************************************* */


  def initializeTour(): Ant = {
    _tour = Vector.fill(numberCities + 1)(Option.empty)
    this
  }

  def initializeVisited(): Ant = {
    _visited = Vector.fill(numberCities + 1)(false)
    this
  }

  def initializeAnt(): Ant = {
    initializeVisited()
    initializeTour()
    this
  }

  def randomInitialPlaceAnt(): Ant = {
    val random: Integer = randomNumber.nextInt(numberCities)
    _tour = _tour.updated(0, Option(random))
    _visited = _visited.updated(random, true)
    this
  }

  def updateTour(step: Int, city: Integer): Ant = {
    _tour = _tour.updated(step, Option(city))
    _visited = _visited.updated(city, true)
    this
  }

  def computeTour(): Ant = {
    _tour = _tour.updated(numberCities, _tour(0))
    _tourLength = computeTourLength(_tour)
    this
  }

  def clone(antToClone: Ant): Unit = {
    antToClone.tourLength = _tourLength
    antToClone._tour = _tour
    antToClone._visited = _visited
  }


}
