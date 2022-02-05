package Entities

import scala.beans.BeanProperty

object Aco {

  private var _numberAnts: Integer = null
  private var _lengthNeighborsList: Integer = null
  private var _ants: Vector[Ant] = Vector.empty


  private var _pheremone: Vector[Vector[Double]] = Vector.empty
  private var _total: Vector[Vector[Double]] = Vector.empty
  private var _probOfSelection: Vector[Double] = Vector.empty
  private var _q0: Double = Double.NaN

  def numberAnts = _numberAnts
  def numberAnts_=(numberAnts:Integer) = {
    _numberAnts = numberAnts
  }

  def lengthNeighborsList = _lengthNeighborsList
  def lengthNeighborsList_=(lengthNeighborsList: Integer) = {
    _lengthNeighborsList = lengthNeighborsList
  }

  def total = _total
  def total_=(total: Vector[Vector[Double]]) = {
    _total = total
  }

  def probOfSelection = _probOfSelection
  def probOfSelection_=(probOfSelection: Vector[Double]) = {
    _probOfSelection = probOfSelection
  }

  def q0 = _q0
  def q0_=(q0: Double) = {
    _q0 = q0
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

  def computeTour(): Unit = {
    _ants = _ants.map(ant => ant.computeTour())
  }

  def constructSolutions(): Unit = {
    initializeAnts()
    randomInitialPlaceAnt()
    (1 until Tsp.numberCities - 1).map((step) => neighbourChooseAndMoveToNext(step))
    computeTour()
  }


}
