package Entities.DistanceStrategies

import Entities.Tsp.nodeptr

class GeoDistance extends DistanceStrategies {

  private val M_PI = 3.14159265358979323846264

  override def computeDistance(i: Int, j: Int): Int = {
    val x1 = nodeptr(i).x
    val x2 = nodeptr(j).x
    val y1 = nodeptr(i).y
    val y2 = nodeptr(j).y

    var deg = x1.toInt.toDouble
    var min = x1 - deg
    val lati = M_PI * (deg + 0.5 * min / 3.0) / 180.0
    deg = x2.toInt.toDouble
    min = x2 - deg
    val latj = M_PI * (deg + 0.5 * min / 3.0) / 180.0

    deg = y1.toInt.toDouble
    min = y1 - deg
    val longi = M_PI * (deg + 0.5 * min / 3.0) / 180.0
    deg = y2.toInt.toDouble
    min = y2 - deg
    val longj = M_PI * (deg + 0.5 * min / 3.0) / 180.0

    val q1 = Math.cos(longi - longj)
    val q2 = Math.cos(lati - latj)
    val q3 = Math.cos(lati + latj)
    (6378.488 * Math.acos(0.5 * ((1.0 + q1) * q2 - (1.0 - q1) * q3)) + 1.0).toInt
  }

}
