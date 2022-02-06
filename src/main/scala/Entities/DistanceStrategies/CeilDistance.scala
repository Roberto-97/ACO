package Entities.DistanceStrategies

import Entities.Tsp.nodeptr

class CeilDistance extends  DistanceStrategies {

  override def computeDistance(i: Int, j: Int): Int = {
    val xd: Double = nodeptr(i).x - nodeptr(j).x
    val yd: Double = nodeptr(i).y - nodeptr(j).y
    Math.ceil(Math.sqrt(xd*xd + yd*yd)).toInt
  }

}
