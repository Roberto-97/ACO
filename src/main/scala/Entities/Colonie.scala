package Entities

class Colonie extends Serializable {

  private var _ants: Vector[Ant] = Vector.empty
  private var _bestSoFarAnt: Ant = null
  private var _restartBestAnt: Ant = null


  private var _pheremone: Array[Array[Double]] = Array.empty
  private var _total: Array[Array[Double]] = Array.empty
  private var _probOfSelection: Vector[Double] = Vector.empty
  private var _foundBranching: Double = Double.NaN
  private var _branchingFactor: Double = Double.NaN
  private var _restartFoundBest: Int = 0
  private var _foundBest: Int = 0
  private var _timeUsed: Double = Double.NaN
  private var _restartIteration: Int = 1


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

  def restartFoundBest = _restartFoundBest

  def restartFoundBest_=(restartFoundBest: Int) = {
    _restartFoundBest = restartFoundBest
  }

  def foundBest = _foundBest

  def foundBest_=(foundBest: Int) = {
    _foundBest = foundBest
  }

  def timeUsed = _timeUsed

  def timeUsed_=(timeUsed: Double) = {
    _timeUsed = timeUsed
  }

  def branchingFactor = _branchingFactor

  def branchingFactor_=(branchingFactor: Double) = {
    _branchingFactor = branchingFactor
  }

  def foundBranching = _foundBranching

  def foundBranching_=(foundBranching: Double) = {
    _foundBranching = foundBranching
  }

  def restartIteration = _restartIteration

  def restartIteration_=(restartIteration: Int) = {
    _restartIteration = restartIteration
  }

  /** ***************************************************************************************************************** */

  def allocateAnts(ep: ExecutionParameters, numberCities: Int): Unit = {
    _ants = Vector.fill(ep.nAnts)(new Ant().initializeTour(numberCities))
    _bestSoFarAnt = new Ant().initializeAnt(numberCities)
    _restartBestAnt = new Ant().initializeAnt(numberCities)
    _probOfSelection = Vector.fill(ep.nnAnts + 1)(0.0)
    _probOfSelection = _probOfSelection.updated(ep.nnAnts, Double.MaxValue)
  }

  def initPheromoneTrails(initialTrail: Double, numberCities: Int): Unit = {
    println("Init trails with " + initialTrail)
    /* Initialize pheromone trails */
    pheremone = Array.fill(numberCities)(Array.fill(numberCities)(initialTrail))
    total = Array.fill(numberCities)(Array.fill(numberCities)(initialTrail))
  }

  def initializeColonie(ep: ExecutionParameters, numberCities: Int): Colonie = {
    this.allocateAnts(ep, numberCities)
    this.bestSoFarAnt.tourLength = Option(Int.MaxValue)
    this
  }

  def computeTotalInformation(ep: ExecutionParameters, numberCities: Int, distance: Vector[Vector[Int]]): Unit = {
    for (i <- 0 until numberCities) {
      for (j <- 0 until i) {
        total(i)(j) = Math.pow(pheremone(i)(j), ep.alpha) * Math.pow(Tsp.heuristic(i, j, distance), ep.beta)
        total(j)(i) = total(i)(j)
      }
    }
  }

  def evaporation(ep: ExecutionParameters, numberCities: Int): Unit = {
    for (i <- 0 until numberCities) {
      for (j <- 0 to i) {
        pheremone(i)(j) = (1 - ep.rho) * pheremone(i)(j)
        pheremone(j)(i) = pheremone(i)(j)
      }
    }
  }

  def globalUpdatePheremone(ant: Ant, numberCities: Int): Unit = {
    val dTau = 1.0 / ant.tourLength.get
    (0 until numberCities).map((i) => {
      val j = ant.tour(i).get
      val h = ant.tour(i + 1).get
      pheremone(j)(h) += dTau
      pheremone(h)(j) = pheremone(j)(h)
    })
  }

  def checkPheromoneTrailLimits(ep: ExecutionParameters, numberCities: Int): Unit = {
    for (i <- 0 until numberCities) {
      for (j <- 0 until i) {
        if (pheremone(i)(j) < ep.trailMin) {
          pheremone(i)(j) = ep.trailMin
          pheremone(j)(i) = ep.trailMin
        } else if (pheremone(i)(j) > ep.trailMax) {
          pheremone(i)(j) = ep.trailMax
          pheremone(j)(j) = ep.trailMax
        }
      }
    }
  }

  def findBest(): Int = {
    var iter = 0
    var i = 0
    var min = _ants(0).tourLength.get
    _ants.map(ant => {
      if (ant.tourLength.get < min) {
        min = ant.tourLength.get
        iter = i
      }
      i += 1
    })
    iter
  }

  def findWorst(): Ant = {
    _ants.maxBy(ant => ant.tourLength)
  }

  def asUpdate(numberCities: Int): Unit = {
    _ants.map(ant => globalUpdatePheremone(ant, numberCities))
  }

  def mmasUpdate(ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    if ((tspParameters.iteration % ep.ugb) != 0) {
      val bestAnt = ants(findBest())
      globalUpdatePheremone(bestAnt, tspParameters.numberCities)
    } else {
      if (ep.ugb == 1 && (tspParameters.iteration - _restartIteration > 50)) globalUpdatePheremone(_bestSoFarAnt, tspParameters.numberCities)
      else globalUpdatePheremone(_restartBestAnt, tspParameters.numberCities)
    }
    ep.ugb = 25
  }

}
