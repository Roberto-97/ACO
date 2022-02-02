package Entities

import scala.beans.BeanProperty

class Ant{
  private var _tour: Vector[Integer] = null
  private var _visited: Boolean = false
  private var _tourLength: Integer = null

  def tour = _tour
  def tour_=(tour:Vector[Integer]) = {
    _tour = tour
  }

}
