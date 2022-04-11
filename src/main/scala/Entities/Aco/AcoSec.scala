package Entities.Aco

import Entities.ExecutionParameters.nAnts
import Entities.Tsp.{numberCities}
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
//    /* Mark all cities as unvisited*/
//    initializeAnts()
//    /* Place the ants on same initial city*/
//    randomInitialPlaceAnt()
//    (1 until numberCities).map((step) => neighbourChooseAndMoveToNext(step))
//    computeTour()
    this.evaluateAnts(aco)
    nTours += nAnts
  }

}
