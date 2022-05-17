package Entities.DistanceStrategies

import Entities.Point

class RoundDistance extends DistanceStrategies with Serializable {

  override def computeDistance(i: Int, j: Int, nodePtr: Vector[Point]): Int = {
    val xd = nodePtr(i).x - nodePtr(j).x
    val yd = nodePtr(i).y - nodePtr(j).y
    (Math.sqrt(xd * xd + yd * yd) + 0.5).toInt
  }

}
