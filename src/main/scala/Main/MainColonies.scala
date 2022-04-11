package Main

import Entities.Aco.{Aco, AcoSec}
import Entities.Ant
import Entities.ExecutionParameters.{coloniesIterations, maxTries}
import Entities.Tsp.computeNearestNeighborsMatrix
import Util.Conf
import Util.InOut._
import Util.SparkConf.{getSparkContext, initializeSparkContext}
import Util.Timer.{elapsedTime, startTimer}
import org.apache.spark.TaskContext

object MainColonies extends Serializable {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    val aco: Aco = new AcoSec()
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
        aco.ants = getSparkContext().parallelize(aco.ants).mapPartitions(iterator => {
          acoColonies(aco, iterator).toIterator
        }).collect().toVector
        aco.updateStatistics()
        aco.pheromoneTrailUpdate()
        iteration += 1
      }
      exitTry(nTry)
    }
    exitProgram(true)
  }

  def acoColonies(aco: Aco, iterator: Iterator[Ant]): Vector[Ant] = {
    val tc = TaskContext.get()
    aco.ants = iterator.toVector
    for (k <- 0 to coloniesIterations) {
      aco.constructSolutions(aco)
      aco.updateStatistics()
      aco.pheromoneTrailUpdate()
      aco.searchControlAndStatistics(nTry)
    }
    aco.ants
  }

}
