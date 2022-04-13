package Entities.Aco

import Entities.Ant
import Entities.ExecutionParameters.nAnts
import Entities.Tsp.numberCities
import Util.InOut.nTours

class AcoSec extends Aco with Serializable {

  def evaluateAnts(aco: Aco): Unit = {
    _ants = _ants.map(ant => {
      /* Mark all cities as unvisited*/
      ant.initializeVisited()
      /* Place the ants on same initial city*/
      ant.randomInitialPlaceAnt()
      /* Choose the nexts cities to visit*/
      (1 until numberCities).map((step) => aco.neighbourChooseAndMoveToNext(step, ant))
      /* Compute tour length*/
      ant.computeTour()
    })
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
