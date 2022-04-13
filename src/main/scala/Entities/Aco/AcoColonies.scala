package Entities.Aco

import Entities.ExecutionParameters._
import Entities.Tsp.numberCities
import Entities.{Ant, Colonie}
import Util.InOut._
import Util.Timer.elapsedTime

class AcoColonies extends Aco {

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


  def updateStatisticsMaster(bestAntColonie: Ant, colonie: Colonie): Unit = {
    if (bestAntColonie.tourLength < colonie.bestSoFarAnt.tourLength) {
      bestAntColonie.clone(colonie.bestSoFarAnt)
      bestAntColonie.clone(colonie.restartBestAnt)
      updateCommonStats(colonie)
    }
    if (bestAntColonie.tourLength < colonie.restartBestAnt.tourLength) {
      bestAntColonie.clone(colonie.restartBestAnt)
      restartFoundBest = iteration
      println("Restart best: " + colonie.restartBestAnt.tourLength + ", restartFoundBest " + restartFoundBest + ", time " + elapsedTime())
    }
  }

  def mmasUpdateMaster(colonie: Colonie): Unit = {
    if ((iteration % ugb) != 0) {
      colonie.globalUpdatePheremone(colonie.bestSoFarAnt)
    } else {
      if (ugb == 1 && (iteration - restartIteration > 50)) colonie.globalUpdatePheremone(colonie.bestSoFarAnt) else colonie.globalUpdatePheremone(colonie.restartBestAnt)
    }
    ugb = 25
  }

  def pheromoneTrailUpdateMaster(colonie: Colonie): Unit = {
    if (asFlag != 0 || mmasFlag != 0) {
      if (lsFlagValues.contains(lsFlag)) {
        /*TODO: LocalSearch*/
      } else {
        colonie.evaporation()
      }
    }

    if (asFlag != 0) {
      colonie.asUpdate()
    } else if (mmasFlag != 0) {
      mmasUpdateMaster(colonie)
    }

    if (mmasFlag != 0 && !lsFlagValues.contains(lsFlag)) {
      colonie.checkPheromoneTrailLimits()
    }

    if (asFlag != 0 || mmasFlag != 0) {
      colonie.computeTotalInformation()
    }

  }

  def searchControlAndStatisticsMaster(nTry: Int, colonie: Colonie): Unit = {
    branchingFactor = nodeBranching(lambda, colonie)
    println("Best so far " + colonie.bestSoFarAnt.tourLength + ", iteration: " + iteration + ", time " + elapsedTime() + ", b_fac " + branchingFactor)
    println("INIT TRAILS !!!")
    colonie.restartBestAnt.tourLength = Int.MaxValue
    colonie.initPheromoneTrails(trailMax)
    colonie.computeTotalInformation()
    restartIteration = iteration
    restartTime = elapsedTime()
    println("try " + nTry + ", iteration " + iteration + ", b-fac " + branchingFactor)
  }

}
