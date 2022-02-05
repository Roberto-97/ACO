package Entities

import scala.beans.BeanProperty
import Entities.Tsp.{computeTourLength, nearestNeighborsMatrix, numberCities}
import Entities.Aco.{lengthNeighborsList, probOfSelection, q0, total}

import scala.util.Random

class Ant{
  private var _tour: Vector[Option[Integer]] = Vector.empty
  private var _visited: Vector[Boolean] = Vector.empty
  private var _tourLength: Integer = null

  def tour = _tour
  def tour_=(tour:Vector[Option[Integer]]) = {
    _tour = tour
  }

  def tourLength = _tourLength
  def tourLength_=(tourLength: Integer) = {
    _tourLength = tourLength
  }

  def initializeAnt(): Ant = {
    _tour = Vector.fill(numberCities)(Option.empty)
    _visited = Vector.fill(numberCities)(false)
    this
  }

  def randomInitialPlaceAnt(): Ant = {
    val r = new scala.util.Random
    val random = r.nextInt(numberCities)
    _tour = _tour.updated(0, Option(random))
    _visited = _visited.updated(random, true)
    this
  }

  def updateTour(step: Int, city: Integer): Unit = {
    _tour = _tour.updated(step, Option(city))
    _visited = _visited.updated(city, true)
  }

  def calculateProb(sumProb: Double, probPtr: Vector[Double], step: Int, currentCity: Integer): Ant = {
    var random = new Random().nextDouble()
    var partialSum = 0.0
    random *= sumProb
    var i = 0
    partialSum = probPtr(i)
    while (partialSum <= random) {
      i+=1
      partialSum += probPtr(i)
    }

    if (i == lengthNeighborsList) {
      return neighbourChooseBestNext(step)
    }
    val help = nearestNeighborsMatrix(currentCity)(i).get
    updateTour(step, help)
    this
  }

  /*
  * chooses for an ant as the next city the one with
  * maximal value of heuristic information times pheromone
  * */
  def chooseBestNext(step: Int): Ant = {
    var nextCity = numberCities
    val currentCity = _tour(step - 1).get
    var valueBest = -1.0
    (0 until numberCities).map((city) => {
      if (!_visited(city)) {
        if (total(currentCity)(city) > valueBest) {
          nextCity = city
          valueBest = total(currentCity)(city)
        }
      }
    })
    updateTour(step, nextCity)
    this
  }

  /*
  * chooses for an ant as the next city the one with
  * maximal value of heuristic information times pheromone
  */
  def neighbourChooseBestNext(step: Int): Ant = {
    var nextCity = numberCities
    val currentCity = _tour(step - 1).get
    var valueBest = -1.0
    (0 until lengthNeighborsList).map((i) => {
      val helpCity = nearestNeighborsMatrix(currentCity)(i).get
      if (!_visited(helpCity)) {
        val help = total(currentCity)(helpCity)
        if (help > valueBest) {
          valueBest = help
          nextCity = helpCity
        }
      }
    })

    if (nextCity == numberCities) {
      return chooseBestNext(step)
    } else {
      updateTour(step, nextCity)
    }
    this
  }

  /*
  * Choose for an ant probabilistically a next city among all unvisited cities
  * in the current city's candidate list. If this is not possible, choose the closest next
  * */
  def neighbourChooseAndMoveToNext(step: Int): Ant = {
    var sumProb = 0.0
    var probPtr = probOfSelection

    if ((q0 > 0.0) && (new Random().nextDouble() < q0)) {
      /* with a probability q0 make the best possible choice according to pheremone trails and heuristic information*/
      return neighbourChooseBestNext(step)
    }

    val currentCity = _tour(step - 1).get
    (0 until lengthNeighborsList).map((i) => {
      if (_visited(nearestNeighborsMatrix(currentCity)(i).get)){
        probPtr = probPtr.updated(i, 0.0) /* City already visited */
      } else {
        probPtr = probPtr.updated(i, total(currentCity)(nearestNeighborsMatrix(currentCity)(i).get))
        sumProb += probPtr(i)
      }
    })

    if (sumProb <= 0.0){
      /*All cities was visited*/
      chooseBestNext(step)
    } else {
      /*At least one neighbor is eligible, chose one according to the selection probabilities*/
      calculateProb(sumProb, probPtr, step, currentCity)
    }
  }
  
  def computeTour(): Ant = {
    _tour = _tour.updated(numberCities, _tour(0))
    _tourLength = computeTourLength(_tour)
    this
  }



}
