package Main


import Entities.{Aco, Ant, LocalSearch, Tsp}
import Util.Conf
import Util.Timer.{elapsedTime, startTimer}

object Main {
  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val ep = conf.build
    println("Sistem parameters " + ep)
    LocalSearch.depthNearestNeighbourList = 2
    Aco.lengthNeighborsList = 4
    Tsp.numberCities = 4
    Tsp.distance = Vector(Vector(0,6,2,1),Vector(6,0,4,8),Vector(2,4,0,1),Vector(1,8,1,0))
    startTimer()
    Tsp.computeNearestNeighborsMatrix()
    val time_used = elapsedTime()

  }


}
