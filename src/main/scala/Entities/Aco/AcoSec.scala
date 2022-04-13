package Entities.Aco

import Entities.ExecutionParameters.nAnts
import Entities.Tsp.numberCities
import Entities.{Ant, Colonie}
import Util.InOut.nTours

class AcoSec extends Aco {

  def evaluateAnts(colonie: Colonie): Unit = {
    colonie.ants = colonie.ants.map(ant => {
      /* Mark all cities as unvisited*/
      ant.initializeVisited()
      /* Place the ants on same initial city*/
      ant.randomInitialPlaceAnt()
      /* Choose the nexts cities to visit*/
      (1 until numberCities).map((step) => neighbourChooseAndMoveToNext(step, ant, colonie))
      /* Compute tour length*/
      ant.computeTour()
    })
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
