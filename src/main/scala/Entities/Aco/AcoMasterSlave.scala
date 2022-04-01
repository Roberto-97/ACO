package Entities.Aco

import Entities.ExecutionParameters.{nAnts, seed}
import Entities.Tsp.{numberCities, randomNumber}
import Util.InOut.nTours
import Util.SparkConf.getSparkContext
import org.apache.spark.TaskContext

import scala.util.Random

class AcoMasterSlave extends Aco with Serializable {

  def evaluateAnts(): Unit = {
    _ants = getSparkContext().parallelize(_ants).map(ant => {
      val tc = TaskContext.get()
      println("TarkContext " + tc.partitionId())
      randomNumber = new Random(seed * (tc.partitionId() + 1))
      /* Mark all cities as unvisited*/
      ant.initializeVisited()
      /* Place the ants on same initial city*/
      ant.randomInitialPlaceAnt()
      /* Choose the nexts cities to visit*/
      (1 until numberCities).map((step) => ant.neighbourChooseAndMoveToNext(step))
      /* Compute tour length*/
      ant.computeTour()
    }).collect().toVector
  }

  def constructSolutions(): Unit = {
    this.evaluateAnts()
    nTours += nAnts
  }


}
