package Main


import Entities.Aco.{Aco, AcoSec}
import Entities.Colonie
import Entities.ExecutionParameters._
import Entities.Tsp.computeNearestNeighborsMatrix
import Util.Conf
import Util.InOut._
import Util.Timer.{elapsedTime, startTimer}

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val aco: Aco = new AcoSec()
    val colonie: Colonie = new Colonie()
    conf.build
    startTimer()
    initProgram(colonie)
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    (0 until maxTries).map(nTry => {
      initTry(nTry, colonie)
      while (!aco.terminationCondition(colonie)) {
        aco.constructSolutions(colonie)
        if (lsFlagValues.contains(lsFlag)) {
          /*TODO: localSearch()*/
        }
        aco.updateStatistics(colonie)
        aco.pheromoneTrailUpdate(colonie)
        aco.searchControlAndStatistics(nTry, colonie)
        iteration += 1
      }
      exitTry(nTry, colonie)
    })
    exitProgram(false)
  }


}
