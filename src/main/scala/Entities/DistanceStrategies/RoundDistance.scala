package Entities.DistanceStrategies

import Entities.Tsp.nodeptr

class RoundDistance extends DistanceStrategies {

  override def computeDistance(i: Int, j: Int): Int = {
    val xd = nodeptr(i).x - nodeptr(j).x
    val yd = nodeptr(i).y - nodeptr(j).y
    (Math.sqrt(xd*xd + yd*yd) + 0.5).toInt
  }

}
