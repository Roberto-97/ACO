package Entities.Aco

import Entities.Ant
import Entities.ExecutionParameters.{nAnts, seed}
import Entities.Tsp.{numberCities, randomNumber}
import Util.InOut.nTours
import Util.SparkConf.getSparkContext
import org.apache.spark.TaskContext

import scala.util.Random


class AcoMasterSlave extends Aco with Serializable {

  def evaluateAnts(aco: Aco): Unit = {
    _ants = getSparkContext().parallelize(_ants).mapPartitions(iterator => {
      val tc = TaskContext.get()
      randomNumber = new Random(seed * (tc.partitionId() + 1))
      iterator.map(ant => {
        /* Mark all cities as unvisited*/
        ant.initializeVisited()
        /* Place the ants on same initial city*/
        ant.randomInitialPlaceAnt()
        /* Choose the nexts cities to visit*/
        (1 until numberCities).map((step) => aco.neighbourChooseAndMoveToNext(step, ant))
        /* Compute tour length*/
        ant.computeTour()
      })
    }).collect().toVector
  }

  def constructSolutions(aco: Aco): Unit = {
    this.evaluateAnts(aco)
    nTours += nAnts
  }

  override def updateStatisticsMaster(bestAntColonie: Ant): Unit = ???

  override def mmasUpdateMaster(): Unit = ???

  override def pheromoneTrailUpdateMaster(): Unit = ???

  override def searchControlAndStatisticsMaster(nTry: Int): Unit = ???
}
