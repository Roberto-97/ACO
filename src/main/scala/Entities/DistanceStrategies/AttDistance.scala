package Entities.DistanceStrategies

import Entities.Point

class AttDistance extends DistanceStrategies with Serializable {

  override def computeDistance(i: Int, j: Int, nodePtr: Vector[Point]): Int = {
    val xd = nodePtr(i).x - nodePtr(j).x
    val yd = nodePtr(i).y - nodePtr(j).y
    val rij = Math.sqrt((xd * xd + yd * yd) / 10.0)
    val tij = rij.toInt.toDouble
    if (tij < rij) tij.toInt + 1 else tij.toInt
  }

}
