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
    conf.build
    initializeSparkContext()
    println("Number colonies -> " + getSparkContext().defaultParallelism)
    startTimer()
    initProgram()
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    val masterColonie = new Colonie().initializeColonie()
    var colonies: Vector[Colonie] = Vector.fill(getSparkContext().defaultParallelism)(new Colonie().initializeColonie())
    println("\nInitialization took " + time_used + " seconds\n")
    for (nTry <- 0 until maxTries) {
      initTry(nTry, colonies)
      masterColonie.bestSoFarAnt.tourLength = Option(Int.MaxValue)
      while (!terminationCondition(masterColonie)) {
        colonies = getSparkContext().parallelize(colonies).mapPartitions(iterator => {
          iterator.map(colonie => {
            executeAcoColonies(masterColonie.bestSoFarAnt, colonie, nTry)
          })
        }).collect().toVector
        val bestColonie = colonies.reduce((c1, c2) => if (c1.bestSoFarAnt.tourLength.get < c2.bestSoFarAnt.tourLength.get) c1 else c2)
        if (bestColonie.bestSoFarAnt.tourLength.get < masterColonie.bestSoFarAnt.tourLength.get) {
          bestColonie.bestSoFarAnt.clone(masterColonie.bestSoFarAnt)
          foundBest = iteration
          timeUsed = elapsedTime()
        }
        println("Best so far " + masterColonie.bestSoFarAnt.tourLength + ", iteration: " + iteration + ", time " + elapsedTime() + ", b_fac " + branchingFactor)
        iteration += 1
      }
      exitTry(nTry, masterColonie)
    }
    exitProgram(true)
  }

  def executeAcoColonies(bestAnt: Ant, colonie: Colonie, nTry: Int): Colonie = {
    bestAnt.clone(colonie.bestSoFarAnt)
    bestAnt.clone(colonie.restartBestAnt)
    for (k <- 0 to coloniesIterations) {
      constructSolutions(colonie)
      updateStatistics(colonie)
      pheromoneTrailUpdate(colonie)
    }
    searchControlAndStatisticsColonie(nTry, colonie)
    colonie
  }
}
