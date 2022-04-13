package Entities

import Entities.ExecutionParameters._
import Entities.Tsp.{heuristic, numberCities}
import Util.InOut.{iteration, restartIteration}

class Colonie extends Serializable {

  private var _ants: Vector[Ant] = Vector.empty
  private var _bestSoFarAnt: Ant = null
  private var _restartBestAnt: Ant = null


  private var _pheremone: Array[Array[Double]] = Array.empty
  private var _total: Array[Array[Double]] = Array.empty
  private var _probOfSelection: Vector[Double] = Vector.empty

  /** *********************************************** Setters && Getters ****************************************************** */

  def ants = _ants

  def ants_=(ants: Vector[Ant]) = {
    _ants = ants
  }

  def bestSoFarAnt = _bestSoFarAnt

  def bestSoFarAnt_=(bestSoFarAnt: Ant) = {
    _bestSoFarAnt = bestSoFarAnt
  }

  def restartBestAnt = _restartBestAnt

  def restartBestAnt_=(restartBestAnt: Ant) = {
    _restartBestAnt = restartBestAnt
  }

  def total = _total

  def total_=(total: Array[Array[Double]]) = {
    _total = total
  }

  def pheremone = _pheremone

  def pheremone_=(pheremone: Array[Array[Double]]) = {
    _pheremone = pheremone
  }

  def probOfSelection = _probOfSelection

  def probOfSelection_=(probOfSelection: Vector[Double]) = {
    _probOfSelection = probOfSelection
  }

  /** ***************************************************************************************************************** */

  def allocateAnts(): Unit = {
    _ants = Vector.fill(nAnts)(new Ant().initializeTour())
    _bestSoFarAnt = new Ant().initializeAnt()
    _restartBestAnt = new Ant().initializeAnt()
    _probOfSelection = Vector.fill(nnAnts + 1)(0.0)
    _probOfSelection = _probOfSelection.updated(nnAnts, Double.MaxValue)
  }

  def initPheromoneTrails(initialTrail: Double): Unit = {
    println("Init trails with " + initialTrail)
    /* Initialize pheromone trails */
    pheremone = Array.fill(numberCities)(Array.fill(numberCities)(initialTrail))
    total = Array.fill(numberCities)(Array.fill(numberCities)(initialTrail))
  }

  def computeTotalInformation(): Unit = {
    for (i <- 0 until numberCities) {
      for (j <- 0 until i) {
        total(i)(j) = Math.pow(pheremone(i)(j), alpha) * Math.pow(heuristic(i, j), beta)
        total(j)(i) = total(i)(j)
      }
    }
  }

  def evaporation(): Unit = {
    for (i <- 0 until numberCities) {
      for (j <- 0 to i) {
        pheremone(i)(j) = (1 - rho) * pheremone(i)(j)
        pheremone(j)(i) = pheremone(i)(j)
      }
    }
  }

  def globalUpdatePheremone(ant: Ant): Unit = {
    val dTau = 1.0 / ant.tourLength
    (0 until numberCities).map((i) => {
      val j = ant.tour(i).get
      val h = ant.tour(i + 1).get
      pheremone(j)(h) += dTau
      pheremone(h)(j) = pheremone(j)(h)
    })
  }

  def checkPheromoneTrailLimits(): Unit = {
    for (i <- 0 until numberCities) {
      for (j <- 0 until i) {
        if (pheremone(i)(j) < trailMin) {
          pheremone(i)(j) = trailMin
          pheremone(j)(i) = trailMin
        } else if (pheremone(i)(j) > trailMax) {
          pheremone(i)(j) = trailMax
          pheremone(j)(j) = trailMax
        }
      }
    }
  }

  def findBest(): Int = {
    var iter = 0
    var i = 0
    var min = _ants(0).tourLength
    _ants.map(ant => {
      if (ant.tourLength < min) {
        min = ant.tourLength
        iter = i
      }
      i += 1
    })
    iter
  }

  def findWorst(): Ant = {
    _ants.maxBy(ant => ant.tourLength)
  }

  def asUpdate(): Unit = {
    _ants.map(ant => globalUpdatePheremone(ant))
  }

  def mmasUpdate(): Unit = {
    if ((iteration % ugb) != 0) {
      val bestAnt = ants(findBest())
      globalUpdatePheremone(bestAnt)
    } else {
      if (ugb == 1 && (iteration - restartIteration > 50)) globalUpdatePheremone(_bestSoFarAnt) else globalUpdatePheremone(_restartBestAnt)
    }
    ugb = 25
  }

}
