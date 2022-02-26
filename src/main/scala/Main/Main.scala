package Main


import Entities.Aco._bestSoFarAnt
import Entities.{Aco, Ant, ExecutionParameters, LocalSearch, Tsp}
import Util.Conf
import Util.Timer.{elapsedTime, startTimer}
import Util.InOut.{_iteration, exitProgram, exitTry, initProgram, initTry}

object Main {


  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    conf.build
    startTimer()
    initProgram()
    Tsp.computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    (0 until ExecutionParameters.maxTries).map(nTry => {
      initTry(nTry)
      while (!Aco.terminationCondition()) {
        Aco.constructSolutions()
        if (ExecutionParameters.lsFlagValues.contains(ExecutionParameters.lsFlag)) {
          /*TODO: localSearch()*/
        }
        Aco.updateStatistics()
        Aco.pheromoneTrailUpdate()
        Aco.searchControlAndStatistics(nTry)
        _iteration += 1
      }
      exitTry(nTry)
    })
    exitProgram()
  }


}
