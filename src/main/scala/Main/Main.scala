package Main


import Entities.{Aco, Ant, LocalSearch, Tsp}
import Util.Timer.{elapsedTime, startTimer}

object Main {
  def main(args: Array[String]): Unit = {
    LocalSearch.depthNearestNeighbourList = 2
    Aco.lengthNeighborsList = 4
    Tsp.numberCities = 4
    Tsp.distance = Vector(Vector(0,6,2,1),Vector(6,0,4,8),Vector(2,4,0,1),Vector(1,8,1,0))
    startTimer()
    Tsp.computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\n Resultado en un tiempo de :" + time_used + " ms")
    println("\n Resultado final: " + Tsp.nearestNeighborsMatrix)
  }


}
