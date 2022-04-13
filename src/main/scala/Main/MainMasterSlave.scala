package Main

import Entities.Aco.{Aco, AcoMasterSlave}
import Entities.Colonie
import Entities.ExecutionParameters.{lsFlag, lsFlagValues, maxTries}
import Entities.Tsp.computeNearestNeighborsMatrix
import Util.Conf
import Util.InOut._
import Util.SparkConf.initializeSparkContext
import Util.Timer.{elapsedTime, startTimer}

object MainMasterSlave {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val aco: Aco = new AcoMasterSlave()
    val colonie: Colonie = new Colonie()
    initializeSparkContext()
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
