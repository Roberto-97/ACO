package Entities.Aco

import Entities.ExecutionParameters.{nAnts}
import Entities.Tsp.numberCities
import Util.InOut.nTours

class AcoSec extends Aco with Serializable {

  def constructSolutions(): Unit = {
    /* Mark all cities as unvisited*/
    initializeAnts()
    /* Place the ants on same initial city*/
    randomInitialPlaceAnt()
    (1 until numberCities).map((step) => neighbourChooseAndMoveToNext(step))
    computeTour()
    nTours += nAnts
  }

}
