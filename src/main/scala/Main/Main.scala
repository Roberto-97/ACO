package Main


import Entities.{Aco, Ant, ExecutionParameters, LocalSearch, Tsp}
import Util.Conf
import Util.Timer.{elapsedTime, startTimer}
import Util.InOut.*
import Entities.Aco.*
import Entities.Tsp.{computeNearestNeighborsMatrix}
import ExecutionParameters.*

object Main {


  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    conf.build
    startTimer()
    initProgram()
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    (0 until maxTries).map(nTry => {
      initTry(nTry)
      while (!terminationCondition()) {
        constructSolutions()
        if (lsFlagValues.contains(lsFlag)) {
          /*TODO: localSearch()*/
        }
        updateStatistics()
        pheromoneTrailUpdate()
        searchControlAndStatistics(nTry)
        iteration += 1
      }
      exitTry(nTry)
    })
    exitProgram()
  }


}
