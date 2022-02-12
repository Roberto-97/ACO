package Entities

import scala.beans.BeanProperty

object Aco {

  private var _ants: Vector[Ant] = Vector.empty
  var _bestSoFarAnt: Ant = new Ant


  private var _pheremone: Vector[Vector[Double]] = Vector.empty
  private var _total: Vector[Vector[Double]] = Vector.empty
  private var _probOfSelection: Vector[Double] = Vector.empty

  def init(numberAnts: Integer): Unit = {
    _ants = Vector.fill(numberAnts)(new Ant)
    _bestSoFarAnt.initializeAnt()
  }

  def total = _total
  def total_=(total: Vector[Vector[Double]]) = {
    _total = total
  }

  def probOfSelection = _probOfSelection
  def probOfSelection_=(probOfSelection: Vector[Double]) = {
    _probOfSelection = probOfSelection
  }

  def initializeAnts(): Unit = {
    _ants = _ants.map(ant => ant.initializeAnt())
  }

  def randomInitialPlaceAnt(): Unit = {
    _ants = _ants.map(ant => ant.randomInitialPlaceAnt())
  }

  def neighbourChooseAndMoveToNext(step: Int): Unit = {
    _ants = _ants.map(ant => ant.neighbourChooseAndMoveToNext(step))
  }

  def chooseClosestNext(step: Int): Unit = {
    _ants = _ants.map(ant => ant.chooseClosestNext(step))
  }

  def computeTour(): Unit = {
    _ants = _ants.map(ant => ant.computeTour())
  }

  def constructSolutions(): Unit = {
    initializeAnts()
    randomInitialPlaceAnt()
    (1 until Tsp.numberCities - 1).map((step) => neighbourChooseAndMoveToNext(step))
    computeTour()
  }

  def nnTour(): Int = {
    initializeAnts()
    randomInitialPlaceAnt()
    (1 until Tsp.numberCities - 1).map((step) => chooseClosestNext(step))
    val step = Tsp.numberCities
    //_ants(0) = _ants(0).tour.updated(Tsp.numberCities, _ants(0).tour(0))
    if (ExecutionParameters.lsFlag != 0) {

    }
    0
  }


}
