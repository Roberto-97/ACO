package Entities

import Entities.Aco.total
import Entities.Tsp.{computeTourLength, numberCities}
import Util.InOut.{_branchingFactor, _foundBest, _foundBranching, _iteration, _lambda, _nTours, _restartFoundBest, _restartIteration, _restartTime, _timeUsed, nodeBranching}
import Util.Timer.elapsedTime

import scala.beans.BeanProperty
import scala.language.postfixOps

object Aco {

  private var _ants: Vector[Ant] = Vector.empty
  var _bestSoFarAnt: Ant = null
  var _restartBestAnt: Ant = null


  private var _pheremone: Array[Array[Double]] = Array.empty
  private var _total: Array[Array[Double]] = Array.empty
  private var _probOfSelection: Vector[Double] = Vector.empty

  def allocateAnts(): Unit = {
    _ants = Vector.fill(ExecutionParameters.nAnts)(new Ant)
    _bestSoFarAnt = new Ant().initializeAnt()
    _restartBestAnt = new Ant().initializeAnt()
    _probOfSelection = Vector.fill(ExecutionParameters.nAnts + 1)(0.0)
    _probOfSelection = _probOfSelection.updated(ExecutionParameters.nAnts, Double.MaxValue)
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
    println("Construct solutions for all ants")
    /* Mark all cities as unvisited*/
    initializeAnts()
    /* Place the ants on same initial city*/
    randomInitialPlaceAnt()
    (1 until Tsp.numberCities - 1).map((step) => neighbourChooseAndMoveToNext(step))
    computeTour()
    _nTours += ExecutionParameters.nAnts
  }

  def initPheromoneTrails(initialTrail: Double): Unit = {
    println("Init trails with " + initialTrail)
    /* Initialize pheromone trails */
    pheremone = Array.fill(numberCities)(Array.fill(numberCities)(initialTrail))
    total = Array.fill(numberCities)(Array.fill(numberCities)(initialTrail))
  }

  def computeTotalInformation(): Unit = {
    println("Compute total information")
    (0 until numberCities).map((i) =>
      (0 until i).map((j) =>
        total(i)(j) = Math.pow(pheremone(i)(j), ExecutionParameters.alpha) * Math.pow(Tsp.heuristic(i, j), ExecutionParameters.beta)
          total(j)(i) = total(i)(j)))
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

  def searchControlAndStatistics(nTry: Int): Unit = {
    println("SEARCH CONTROL AND STATISTICS")
    if (_iteration % 100 == 0) {
      val branchingFactor = nodeBranching(_lambda)
      println("Best so far " + _bestSoFarAnt.tourLength + ", iteration: " + _iteration + ", time "+ elapsedTime() + ", b_fac " + branchingFactor)
      if (ExecutionParameters.mmasFlag != 0 && (branchingFactor < ExecutionParameters.branchFac) && (_iteration - _restartFoundBest > 250)) {
        println("INIT TRAILS !!!")
        _restartBestAnt.tourLength = Int.MaxValue
        initPheromoneTrails(ExecutionParameters.trailMax)
        computeTotalInformation()
        _restartIteration = _iteration
        _restartTime = elapsedTime()
      }
      println("try " + nTry + ", iteration " + _iteration + ", b-fac " + branchingFactor)
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

  def terminationCondition(): Boolean = {
    _nTours >= ExecutionParameters.maxTours &&
      (elapsedTime() >= ExecutionParameters.maxTime || _bestSoFarAnt.tourLength <= ExecutionParameters.optimal)
  }

  def updateStatistics(): Unit = {
    var iterationBestAntIter: Integer = null
    iterationBestAntIter = findBest()
    if (_ants(iterationBestAntIter).tourLength < _bestSoFarAnt.tourLength) {
      _timeUsed = elapsedTime()
      _bestSoFarAnt = _ants(iterationBestAntIter)
      _restartBestAnt = _ants(iterationBestAntIter)
      _foundBest = _iteration
      _restartFoundBest = _iteration
      _foundBranching = nodeBranching(_lambda)
      _branchingFactor = _foundBranching
      if (ExecutionParameters.mmasFlag != 0) {
        if (ExecutionParameters.lsFlag != 0) {
          val px = Math.exp(Math.log(0.05)/numberCities)
          ExecutionParameters.trailMin = 1.0 * (1 - px) / (px * ((ExecutionParameters.nnAnts + 1) / 2))
          ExecutionParameters.trailMax = 1.0 / (ExecutionParameters.rho * _bestSoFarAnt.tourLength)
          ExecutionParameters.trail0 = ExecutionParameters.trailMax
          ExecutionParameters.trailMin = ExecutionParameters.trailMax * ExecutionParameters.trailMin
        } else {
          ExecutionParameters.trailMax = 1.0 / (ExecutionParameters.rho * _bestSoFarAnt.tourLength)
          ExecutionParameters.trailMin = ExecutionParameters.trailMax / (2 * numberCities)
          ExecutionParameters.trail0 = ExecutionParameters.trailMax
        }
      }
    }
    if (_ants(iterationBestAntIter).tourLength < _restartBestAnt.tourLength) {
      _restartBestAnt = _ants(iterationBestAntIter)
      _restartFoundBest = _iteration
      println("Restart best: " + _restartBestAnt.tourLength + ", restartFoundBest " + _restartFoundBest + ", time " + elapsedTime())
    }
  }

  def evaporation(): Unit = {
    println("Pheromone evaporation")
    (0 until numberCities).map((i) =>
      (0 to i).map((j) => {
        pheremone(i)(j) = (1 - ExecutionParameters.rho) * pheremone(i)(j)
        pheremone(j)(i) = pheremone(i)(j)
      }))
  }

  def globalUpdatePheremone(ant: Ant, weigth: Double = 1.0): Unit = {
    println("Global pheromone update")
    val dTau = weigth / ant.tourLength
    (0 until numberCities - 1).map((i) => {
      val j = ant.tour(i).get
      val h = ant.tour(i + 1).get
      pheremone(j)(h) += dTau
      pheremone(h)(j) = pheremone(j)(h)
    })
  }

  def checkPheromoneTrailLimits(): Unit = {
    println("Mmas specific: check pheromone trail limits")
    (0 until numberCities).map(i => (0 until i).map(j => {
      if (pheremone(i)(j) < ExecutionParameters.trailMin) {
        pheremone(i)(j) = ExecutionParameters.trailMin
        pheremone(j)(i) = ExecutionParameters.trailMin
      } else if (pheremone(i)(j) > ExecutionParameters.trailMax) {
        pheremone(i)(j) = ExecutionParameters.trailMax
        pheremone(i)(j) = ExecutionParameters.trailMax
      }
    }))
  }

  def asUpdate(): Unit = {
    println("Ant System pheromone deposit")
    _ants.map(ant => globalUpdatePheremone(ant))
  }

  def mmasUpdate(): Unit = {
    println("MAX-MIN Ant System pheromone deposit")
    if ((_iteration % ExecutionParameters.ugb) != 0) {
      val bestAnt = _ants(findBest())
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

    if (ExecutionParameters.mmasFlag != 0 && ExecutionParameters.lsFlag == 0) {
      checkPheromoneTrailLimits()
    }

    if (ExecutionParameters.asFlag != 0 || ExecutionParameters.mmasFlag != 0) {
      computeTotalInformation()
    }

  }


}
