package Entities

import Entities.Tsp.{_iteration, _nTours, numberCities}

import scala.beans.BeanProperty

object Aco {

  private var _ants: Vector[Ant] = Vector.empty
  var _bestSoFarAnt: Ant = new Ant
  var _restartBestAnt: Ant = new Ant


  private var _pheremone: Vector[Vector[Double]] = Vector.empty
  private var _total: Vector[Vector[Double]] = Vector.empty
  private var _probOfSelection: Vector[Double] = Vector.empty

  def init(numberAnts: Integer): Unit = {
    _ants = Vector.fill(numberAnts)(new Ant)
    _bestSoFarAnt.initializeAnt()
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

  def chooseClosestNext(step: Int): Unit = {
    _ants = _ants.map(ant => ant.chooseClosestNext(step))
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
    total = (0 to numberCities).map((i) =>
      (0 to i).map((j) =>
        Math.pow(pheremone(i)(j), ExecutionParameters.alpha) * Math.pow(Tsp.heuristic(i, j), ExecutionParameters.beta)).toVector)
      .toVector
  }

  def nnTour(): Int = {
    initializeAnts()
    randomInitialPlaceAnt()
    (1 until Tsp.numberCities - 1).map((step) => chooseClosestNext(step))
    val step = Tsp.numberCities
    //_ants(0) = _ants(0).tour.updated(Tsp.numberCities, _ants(0).tour(0))
    if (ExecutionParameters.lsFlag != 0) {
    }
    0
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

  def easUpdate(): Unit = {
    _ants.map(ant => globalUpdatePheremone(ant, ExecutionParameters.elitistsAnts))
  }

  def bwasWorstAntUpdate(worstAnt: Ant, bestAnt: Ant): Unit = {
    val pos2 = Vector.fill(numberCities)(0)
  }

  def rasUpdate(): Unit = {
    var helpVector = _ants
    (0 until ExecutionParameters.rasRanks - 1).map((i) => {
      val antMin = helpVector.minByOption(ant => ant.tourLength)
      if (!antMin.isEmpty) {
        globalUpdatePheremone(antMin.get, ExecutionParameters.rasRanks - i - 1)
        val index = helpVector.indexOf(antMin.get)
        helpVector = helpVector.slice(0, index) ++ helpVector.drop(index + 1)
      }
    })
    globalUpdatePheremone(_bestSoFarAnt, ExecutionParameters.rasRanks)
  }

  def mmasUpdate(): Unit = {
    if ((Tsp._iteration % ExecutionParameters.ugb) != 0) {
      val bestAnt = findBest()
      globalUpdatePheremone(bestAnt)
    } else {
      if (ExecutionParameters.ugb == 1 && (_iteration - Tsp._restartIteration)) globalUpdatePheremone(_bestSoFarAnt) else globalUpdatePheremone(_restartBestAnt)
    }
    ExecutionParameters.ugb = 25
  }

  def bwasUpdate(): Unit = {
    globalUpdatePheremone(_bestSoFarAnt)
    val worstAnt = findWorst()


  }

  def pheromoneTrailUpdate(): Unit = {
    if (ExecutionParameters.asFlag != 0 || ExecutionParameters.easFlag != 0 || ExecutionParameters.rasFlag != 0 ||
    ExecutionParameters.bwasFlag != 0 || ExecutionParameters.mmasFlag != 0) {
      if (ExecutionParameters.lsFlag != 0) {
        /*TODO: LocalSearc*/
      } else {
        evaporation()
      }
    }

  }


}
