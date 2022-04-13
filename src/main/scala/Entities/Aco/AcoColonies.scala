package Entities.Aco

import Entities.Ant
import Entities.ExecutionParameters._
import Entities.Tsp.numberCities
import Util.InOut._
import Util.Timer.elapsedTime

class AcoColonies extends Aco with Serializable {

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


  def updateStatisticsMaster(bestAntColonie: Ant): Unit = {
    if (bestAntColonie.tourLength < _bestSoFarAnt.tourLength) {
      bestAntColonie.clone(_bestSoFarAnt)
      bestAntColonie.clone(_restartBestAnt)
      updateCommonStats()
    }
    if (bestAntColonie.tourLength < _restartBestAnt.tourLength) {
      bestAntColonie.clone(_restartBestAnt)
      restartFoundBest = iteration
      println("Restart best: " + _restartBestAnt.tourLength + ", restartFoundBest " + restartFoundBest + ", time " + elapsedTime())
    }
  }

  def mmasUpdateMaster(): Unit = {
    if ((iteration % ugb) != 0) {
      globalUpdatePheremone(_bestSoFarAnt)
    } else {
      if (ugb == 1 && (iteration - restartIteration > 50)) globalUpdatePheremone(_bestSoFarAnt) else globalUpdatePheremone(_restartBestAnt)
    }
    ugb = 25
  }

  def pheromoneTrailUpdateMaster(): Unit = {
    if (asFlag != 0 || mmasFlag != 0) {
      if (lsFlagValues.contains(lsFlag)) {
        /*TODO: LocalSearch*/
      } else {
        evaporation()
      }
    }

    if (asFlag != 0) {
      asUpdate()
    } else if (mmasFlag != 0) {
      mmasUpdateMaster()
    }

    if (mmasFlag != 0 && !lsFlagValues.contains(lsFlag)) {
      checkPheromoneTrailLimits()
    }

    if (asFlag != 0 || mmasFlag != 0) {
      computeTotalInformation()
    }

  }

  def searchControlAndStatisticsMaster(nTry: Int): Unit = {
    branchingFactor = nodeBranching(lambda)
    println("Best so far " + _bestSoFarAnt.tourLength + ", iteration: " + iteration + ", time " + elapsedTime() + ", b_fac " + branchingFactor)
    println("INIT TRAILS !!!")
    _restartBestAnt.tourLength = Int.MaxValue
    initPheromoneTrails(trailMax)
    computeTotalInformation()
    restartIteration = iteration
    restartTime = elapsedTime()
    println("try " + nTry + ", iteration " + iteration + ", b-fac " + branchingFactor)
  }

}
