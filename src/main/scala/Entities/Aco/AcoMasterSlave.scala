package Entities.Aco

import Entities.ExecutionParameters.nAnts
import Entities.Tsp.numberCities
import Util.InOut.nTours
import Util.SparkConf.getSparkContext

class AcoMasterSlave extends Aco with Serializable {

  def evaluateAnts(): Unit = {
    _ants = getSparkContext().parallelize(_ants).map(ant => {
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
