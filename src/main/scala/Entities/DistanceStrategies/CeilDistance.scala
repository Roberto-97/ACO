package Entities.DistanceStrategies

import Entities.Point

class CeilDistance extends DistanceStrategies with Serializable {

  override def computeDistance(i: Int, j: Int, nodePtr: Vector[Point]): Int = {
    val xd: Double = nodePtr(i).x - nodePtr(j).x
    val yd: Double = nodePtr(i).y - nodePtr(j).y
    Math.ceil(Math.sqrt(xd * xd + yd * yd)).toInt
  }

}
