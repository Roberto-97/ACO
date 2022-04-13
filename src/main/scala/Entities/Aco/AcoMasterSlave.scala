package Entities.Aco

import Entities.ExecutionParameters.{nAnts, seed}
import Entities.Tsp.{numberCities, randomNumber}
import Entities.{Ant, Colonie}
import Util.InOut.nTours
import Util.SparkConf.getSparkContext
import org.apache.spark.TaskContext

import scala.util.Random


class AcoMasterSlave extends Aco {

  def evaluateAnts(colonie: Colonie): Unit = {
    colonie.ants = getSparkContext().parallelize(colonie.ants).mapPartitions(iterator => {
      val tc = TaskContext.get()
      randomNumber = new Random(seed * (tc.partitionId() + 1))
      iterator.map(ant => {
        /* Mark all cities as unvisited*/
        ant.initializeVisited()
        /* Place the ants on same initial city*/
        ant.randomInitialPlaceAnt()
        /* Choose the nexts cities to visit*/
        (1 until numberCities).map((step) => neighbourChooseAndMoveToNext(step, ant, colonie))
        /* Compute tour length*/
        ant.computeTour()
      })
    }).collect().toVector
  }

  def constructSolutions(colonie: Colonie): Unit = {
    this.evaluateAnts(colonie)
    nTours += nAnts
  }

  override def updateStatisticsMaster(bestAntColonie: Ant, colonie: Colonie): Unit = ???

  override def mmasUpdateMaster(colonie: Colonie): Unit = ???

  override def pheromoneTrailUpdateMaster(colonie: Colonie): Unit = ???

  override def searchControlAndStatisticsMaster(nTry: Int, colonie: Colonie): Unit = ???
}
