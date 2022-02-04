package Entities

import scala.beans.BeanProperty

object Aco {

  private var _numberAnts: Integer = null
  private var _lengthNeighborsList: Integer = null


  private var _pheremone: Vector[Vector[Double]] = Vector.empty
  private var _total: Vector[Vector[Double]] = Vector.empty

  def numberAnts = _numberAnts
  def numberAnts_=(numberAnts:Integer) = {
    _numberAnts = numberAnts
  }

  def lengthNeighborsList = _lengthNeighborsList
  def lengthNeighborsList_=(lengthNeighborsList: Integer) = {
    _lengthNeighborsList = lengthNeighborsList
  }


}
