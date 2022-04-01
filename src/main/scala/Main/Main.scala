package Main


import Entities._
import Util.Conf
import Util.Timer.{elapsedTime, startTimer}
import Entities.Aco.{Aco, AcoSec}
import Util.InOut._
import Entities.Tsp.computeNearestNeighborsMatrix
import ExecutionParameters._

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val aco: Aco = new AcoSec()
    conf.build
    startTimer()
    initProgram(aco)
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    (0 until maxTries).map(nTry => {
      initTry(nTry)
      while (!aco.terminationCondition()) {
        aco.constructSolutions(aco)
        if (lsFlagValues.contains(lsFlag)) {
          /*TODO: localSearch()*/
        }
        aco.updateStatistics()
        aco.pheromoneTrailUpdate()
        aco.searchControlAndStatistics(nTry)
        iteration += 1
      }
      exitTry(nTry)
    })
    exitProgram()
  }


}
