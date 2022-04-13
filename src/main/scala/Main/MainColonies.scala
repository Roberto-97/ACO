package Main

import Entities.Aco.{Aco, AcoColonies}
import Entities.Ant
import Entities.ExecutionParameters.{coloniesIterations, maxTries}
import Entities.Tsp.computeNearestNeighborsMatrix
import Util.Conf
import Util.InOut._
import Util.SparkConf.{getSparkContext, initializeSparkContext}
import Util.Timer.{elapsedTime, startTimer}

object MainColonies extends Serializable {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val aco: Aco = new AcoColonies()
    initializeSparkContext()
    conf.build
    startTimer()
    initProgram(aco)
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    for (nTry <- 0 until maxTries) {
      initTry(nTry)
      while (!aco.terminationCondition()) {
        val bestAntColonie = getSparkContext().parallelize(aco.ants).mapPartitions(iterator => {
          executeAcoColonies(aco, iterator).toIterator
        }).reduce((ant1, ant2) => if (ant1.tourLength < ant2.tourLength) ant1 else ant2)
        aco.updateStatisticsMaster(bestAntColonie)
        aco.pheromoneTrailUpdateMaster()
        aco.searchControlAndStatisticsMaster(nTry)
        iteration += 1
      }
      exitTry(nTry)
    }
    exitProgram(true)
  }

  def executeAcoColonies(aco: Aco, iterator: Iterator[Ant]): Vector[Ant] = {
    aco.ants = iterator.toVector
    for (k <- 0 to coloniesIterations) {
      aco.constructSolutions(aco)
      aco.updateStatistics()
      aco.pheromoneTrailUpdate()
    }
    Vector(aco.bestSoFarAnt)
  }

}
