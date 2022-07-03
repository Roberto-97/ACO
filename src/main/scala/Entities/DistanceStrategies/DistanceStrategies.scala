package Entities.DistanceStrategies

import Entities.Point

trait DistanceStrategies {

  def computeDistance(i: Int, j: Int, nodePtr: Vector[Point]): Int

}
