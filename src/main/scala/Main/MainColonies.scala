package Main

import Entities.Aco.AcoOperations._
import Entities.ExecutionParameters.{coloniesIterations, maxTries}
import Entities.Tsp.computeNearestNeighborsMatrix
import Entities.{Ant, Colonie}
import Util.Conf
import Util.InOut._
import Util.SparkConf.{getSparkContext, initializeSparkContext}
import Util.Timer.{elapsedTime, startTimer}

object MainColonies extends Serializable {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val colonie: Colonie = new Colonie()
    initializeSparkContext()
    conf.build
    startTimer()
    initProgram(colonie)
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    for (nTry <- 0 until maxTries) {
      initTry(nTry, colonie)
      while (!terminationCondition(colonie)) {
        val bestAntColonie = getSparkContext().parallelize(colonie.ants).mapPartitions(iterator => {
          executeAcoColonies(iterator, colonie).toIterator
        }).reduce((ant1, ant2) => if (ant1.tourLength < ant2.tourLength) ant1 else ant2)
        updateStatisticsMaster(bestAntColonie, colonie)
        pheromoneTrailUpdateMaster(colonie)
        searchControlAndStatisticsMaster(nTry, colonie)
        iteration += 1
      }
      exitTry(nTry, colonie)
    }
    exitProgram(true)
  }

  def executeAcoColonies(iterator: Iterator[Ant], colonie: Colonie): Vector[Ant] = {
    colonie.ants = iterator.toVector
    for (k <- 0 to coloniesIterations) {
      constructSolutions(colonie)
      updateStatistics(colonie)
      pheromoneTrailUpdate(colonie)
    }
    Vector(colonie.bestSoFarAnt)
  }

}
