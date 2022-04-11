package Main

import Entities.Aco.{Aco, AcoMasterSlave}
import Util.InOut._
import Entities.ExecutionParameters.{lsFlag, lsFlagValues, maxTries}
import Entities.Tsp.computeNearestNeighborsMatrix
import Util.Conf
import Util.InOut.initProgram
import Util.SparkConf.initializeSparkContext
import Util.Timer.{elapsedTime, startTimer}

object MainMasterSlave {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val aco: Aco = new AcoMasterSlave()
    initializeSparkContext()
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
