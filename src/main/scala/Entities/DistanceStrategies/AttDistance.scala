package Entities.DistanceStrategies

import Entities.Tsp.nodeptr

class AttDistance extends DistanceStrategies {

  override def computeDistance(i: Int, j: Int): Int = {
    val xd = nodeptr(i).x - nodeptr(j).x
    val yd = nodeptr(i).y - nodeptr(j).y
    val rij = Math.sqrt((xd * xd + yd * yd) / 10.0)
    val tij = rij.toInt.toDouble
    if (tij < rij) tij.toInt + 1 else tij.toInt
  }

}
