package Entities

import Entities.Tsp.{computeTourLength, numberCities}
import Util.InOut.{_iteration, _lambda, _nTours, _restartIteration, _restartTime, nodeBranching}
import Util.Timer.elapsedTime

import scala.beans.BeanProperty
import scala.language.postfixOps

object Aco {

  private var _ants: Vector[Ant] = Vector.empty
  var _bestSoFarAnt: Ant = new Ant
  var _restartBestAnt: Ant = new Ant


  private var _pheremone: Vector[Vector[Double]] = Vector.empty
  private var _total: Vector[Vector[Double]] = Vector.empty
  private var _probOfSelection: Vector[Double] = Vector.empty

  def allocateAnts(): Unit = {
    _ants = Vector.fill(ExecutionParameters.nnAnts)(new Ant)
    _bestSoFarAnt.initializeAnt()
    _restartBestAnt.initializeAnt()
    _probOfSelection = Vector.fill(ExecutionParameters.nnAnts + 1)(0.0)
    _probOfSelection = _probOfSelection.updated(ExecutionParameters.nnAnts, Double.MaxValue)
  }

  def total = _total
  def total_=(total: Vector[Vector[Double]]) = {
    _total = total
  }

  def pheremone = _pheremone
  def pheremone_=(pheremone: Vector[Vector[Double]]) = {
    _pheremone = pheremone
  }

  def probOfSelection = _probOfSelection
  def probOfSelection_=(probOfSelection: Vector[Double]) = {
    _probOfSelection = probOfSelection
  }

  def initializeAnts(): Unit = {
    _ants = _ants.map(ant => ant.initializeAnt())
  }

  def randomInitialPlaceAnt(): Unit = {
    _ants = _ants.map(ant => ant.randomInitialPlaceAnt())
  }

  def neighbourChooseAndMoveToNext(step: Int): Unit = {
    _ants = _ants.map(ant => ant.neighbourChooseAndMoveToNext(step))
  }

  def computeTour(): Unit = {
    _ants = _ants.map(ant => ant.computeTour())
  }

  def constructSolutions(): Unit = {
    initializeAnts()
    randomInitialPlaceAnt()
    (1 until Tsp.numberCities - 1).map((step) => neighbourChooseAndMoveToNext(step))
    computeTour()
  }

  def initPheromoneTrails(initialTrail: Double): Unit = {
    pheremone = Vector.fill(numberCities)(Vector.fill(numberCities)(initialTrail))
    total = Vector.fill(numberCities)(Vector.fill(numberCities)(initialTrail))
  }

  def computeTotalInformation(): Unit = {
    (0 until numberCities).map((i) =>
      (0 until i).map((j) =>
        total = total.updated(i, total(i).updated(j, Math.pow(pheremone(i)(j), ExecutionParameters.alpha) * Math.pow(Tsp.heuristic(i, j), ExecutionParameters.beta)))
        total = total.updated(j, total(j).updated(i, total(i)(j)))
        ))
  }

  def checkPheromoneTrailLimits(): Unit = {
    (0 until numberCities).map((i) =>
      (0 until i).map((j) => {
        if (pheremone(i)(j) < ExecutionParameters.trailMin) {
          pheremone = pheremone.updated(i, pheremone(i).updated(j, ExecutionParameters.trailMin))
          pheremone = pheremone.updated(j, pheremone(j).updated(i, ExecutionParameters.trailMin))
        } else if (pheremone(i)(j) > ExecutionParameters.trailMax) {
          pheremone = pheremone.updated(i, pheremone(i).updated(j, ExecutionParameters.trailMax))
          pheremone = pheremone.updated(j, pheremone(j).updated(i, ExecutionParameters.trailMax))
        }
      })
    )
  }

  def nnTour(): Int = {
    _ants(0).initializeAnt()
    _ants(0).randomInitialPlaceAnt()

    (1 until Tsp.numberCities - 1).map((step) => {
      _ants(0).chooseClosestNext(step)
    })
    _ants(0).tour = _ants(0).tour.updated(numberCities - 1, _ants(0).tour(0))
    if (ExecutionParameters.lsFlag != 0) {
      /*TODO: twoOptFirst()*/
    }
    _nTours += 1
    _ants(0).tourLength = computeTourLength(_ants(0).tour)
    val result = _ants(0).tourLength
    _ants(0).initializeAnt()
    result
  }

  def searchControlAndStatistics(): Unit = {
    if (_iteration % 100 == 0) {
      val branchingFactor = nodeBranching(_lambda)
      _restartBestAnt.tourLength = Int.MaxValue
      initPheromoneTrails(ExecutionParameters.trailMax)
      computeTotalInformation()
      _restartIteration = _iteration
      _restartTime = elapsedTime()
    }
  }

  def findBest(): Ant = {
    _ants.minBy(ant => ant.tourLength)
  }

  def findWorst(): Ant = {
    _ants.maxBy(ant => ant.tourLength)
  }

  def terminationCondition(): Boolean = {
    _nTours >= ExecutionParameters.maxTours && _bestSoFarAnt.tourLength <= ExecutionParameters.optimal
  }

  def updateStatistics(): Unit = {
    var iterationBestAnt: Ant = null
    val px: Double = 0.0
    iterationBestAnt = findBest()
    if (iterationBestAnt.tourLength < _bestSoFarAnt.tourLength) {
      _bestSoFarAnt = iterationBestAnt
      _restartBestAnt = iterationBestAnt
    }
    if (iterationBestAnt.tourLength < _restartBestAnt.tourLength) {
      _restartBestAnt = iterationBestAnt
    }
  }

  def evaporation(): Unit = {
    (0 until numberCities).map((i) =>
      (0 to i).map((j) => {
        pheremone = pheremone.updated(i, pheremone(i).updated(j, (1 - ExecutionParameters.rho) * pheremone(i)(j)))
        pheremone = pheremone.updated(j, pheremone(j).updated(i, pheremone(i)(j)))
      }))
  }

  def globalUpdatePheremone(ant: Ant, weigth: Double = 1.0): Unit = {
    val dTau = weigth / ant.tourLength
    (0 until numberCities).map((i) => {
      val j = ant.tour(i).get
      val h = ant.tour(i + 1).get
      pheremone = pheremone.updated(j, pheremone(j).updated(h, pheremone(j)(h) + dTau))
      pheremone = pheremone.updated(h, pheremone(h).updated(j, pheremone(j)(h)))
    })
  }

  def asUpdate(): Unit = {
    _ants.map(ant => globalUpdatePheremone(ant))
  }

  def mmasUpdate(): Unit = {
    if ((_iteration % ExecutionParameters.ugb) != 0) {
      val bestAnt = findBest()
      globalUpdatePheremone(bestAnt)
    } else {
      if (ExecutionParameters.ugb == 1 && (_iteration - _restartIteration > 50)) globalUpdatePheremone(_bestSoFarAnt) else globalUpdatePheremone(_restartBestAnt)
    }
    ExecutionParameters.ugb = 25
  }

  def pheromoneTrailUpdate(): Unit = {
    if (ExecutionParameters.asFlag != 0 || ExecutionParameters.mmasFlag != 0) {
      if (ExecutionParameters.lsFlag != 0) {
        /*TODO: LocalSearch*/
      } else {
        evaporation()
      }
    }

    if (ExecutionParameters.asFlag != 0) {
      asUpdate()
    } else if (ExecutionParameters.mmasFlag != 0){
      mmasUpdate()
    }

    if (ExecutionParameters.asFlag != 0 || ExecutionParameters.mmasFlag != 0) {
      computeTotalInformation()
    }

  }


}
