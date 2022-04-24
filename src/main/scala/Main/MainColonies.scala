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
    initializeSparkContext()
    conf.build
    startTimer()
    initProgram()
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    val masterColonie = new Colonie()
    var colonies: Vector[Colonie] = Vector.fill(getSparkContext().defaultParallelism)(new Colonie())
    println("\nInitialization took " + time_used + " seconds\n")
    for (nTry <- 0 until maxTries) {
      initTry(nTry, colonies)
      masterColonie.bestSoFarAnt.tourLength = Int.MaxValue
      while (!terminationCondition(masterColonie)) {
        colonies = getSparkContext().parallelize(colonies).mapPartitions(iterator => {
          iterator.map(colonie => {
            executeAcoColonies(masterColonie.bestSoFarAnt, colonie, nTry)
          })
        }).collect().toVector
        val bestColonie = colonies.reduce((c1, c2) => if (c1.bestSoFarAnt.tourLength < c2.bestSoFarAnt.tourLength) c1 else c2)
        bestColonie.bestSoFarAnt.clone(masterColonie.bestSoFarAnt)
        println("Best so far " + masterColonie.bestSoFarAnt.tourLength + ", iteration: " + iteration + ", time " + elapsedTime() + ", b_fac " + branchingFactor)
        iteration += 1
      }
      exitTry(nTry, masterColonie)
    }
    exitProgram(true)
  }

  def executeAcoColonies(bestAnt: Ant, colonie: Colonie, nTry: Int): Colonie = {
    bestAnt.clone(colonie.bestSoFarAnt)
    for (k <- 0 to coloniesIterations) {
      constructSolutions(colonie)
      updateStatistics(colonie)
      pheromoneTrailUpdate(colonie)
    }
    searchControlAndStatisticsColonie(nTry, colonie)
    colonie
  }
}
