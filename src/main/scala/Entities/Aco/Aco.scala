package Entities.Aco

import Entities.Ant
import Entities.ExecutionParameters._
import Entities.Tsp._
import Util.InOut._
import Util.Timer.elapsedTime

trait Aco {

  protected var _ants: Vector[Ant] = Vector.empty
  protected var _bestSoFarAnt: Ant = null
  protected var _restartBestAnt: Ant = null


  private var _pheremone: Array[Array[Double]] = Array.empty
  private var _total: Array[Array[Double]] = Array.empty
  protected var _probOfSelection: Vector[Double] = Vector.empty

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

  /** ************************************************************************************************************************* */

  def allocateAnts(): Unit = {
    _ants = Vector.fill(nAnts)(new Ant().initializeTour())
    _bestSoFarAnt = new Ant().initializeAnt()
    _restartBestAnt = new Ant().initializeAnt()
    _probOfSelection = Vector.fill(nnAnts + 1)(0.0)
    _probOfSelection = _probOfSelection.updated(nnAnts, Double.MaxValue)
  }

  def initializeAnts(): Unit = {
    _ants = _ants.map(ant => ant.initializeVisited())
  }

  def randomInitialPlaceAnt(): Unit = {
    _ants = _ants.map(ant => ant.randomInitialPlaceAnt())
  }

//  def neighbourChooseAndMoveToNext(step: Int): Unit = {
//    _ants = _ants.map(ant => ant.neighbourChooseAndMoveToNext(step))
//  }

  def computeTour(): Unit = {
    _ants = _ants.map(ant => ant.computeTour())
  }

  def constructSolutions(aco: Aco)

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

  def calculateProb(sumProb: Double, probPtr: Vector[Double], step: Int, currentCity: Integer, ant: Ant): Ant = {
    var random = randomNumber.nextDouble()
    random *= sumProb
    var i = 0
    var partialSum = probPtr(i)
    while (partialSum <= random) {
      i+=1
      partialSum += probPtr(i)
    }

    if (i == nnAnts) {
      return neighbourChooseBestNext(step, ant)
    }
    val help = nearestNeighborsMatrix(currentCity)(i).get
    ant.updateTour(step, help)
  }

  /*
  * chooses for an ant as the next city the one with
  * maximal value of heuristic information times pheromone
  * */
  def chooseBestNext(step: Int, ant: Ant): Ant = {
    var nextCity = numberCities
    val currentCity = ant.tour(step - 1).get
    var valueBest = -1.0
    (0 until numberCities).map((city) => {
      if (!ant.visited(city)) {
        if (total(currentCity)(city) > valueBest) {
          nextCity = city
          valueBest = total(currentCity)(city)
        }
      }
    })
    ant.updateTour(step, nextCity)
  }

  /*
  * chooses for an ant as the next city the one with
  * maximal value of heuristic information times pheromone
  */
  def neighbourChooseBestNext(step: Int, ant: Ant): Ant = {
    var nextCity = numberCities
    val currentCity = ant.tour(step - 1).get
    var valueBest = -1.0
    (0 until nnAnts).map((i) => {
      val helpCity = nearestNeighborsMatrix(currentCity)(i).get
      if (!ant.visited(helpCity)) {
        val help = total(currentCity)(helpCity)
        if (help > valueBest) {
          valueBest = help
          nextCity = helpCity
        }
      }
    })

    if (nextCity == numberCities) {
      chooseBestNext(step, ant)
    } else {
      ant.updateTour(step, nextCity)
    }
  }

  /*
  * Choose for an ant probabilistically a next city among all unvisited cities
  * in the current city's candidate list. If this is not possible, choose the closest next
  * */
  def neighbourChooseAndMoveToNext(step: Int, ant: Ant): Ant = {
    var sumProb = 0.0
    var probPtr = Vector.fill(nnAnts + 1)(0.0).updated(nnAnts, Double.MaxValue)

    if ((q0 > 0.0) && (randomNumber.nextDouble() < q0)) {
      /* with a probability q0 make the best possible choice according to pheremone trails and heuristic information*/
      return neighbourChooseBestNext(step, ant)
    }

    val currentCity = ant.tour(step - 1).get
    (0 until nnAnts).map((i) => {
      if (ant.visited(nearestNeighborsMatrix(currentCity)(i).get)){
        probPtr = probPtr.updated(i, 0.0) /* City already visited */
      } else {
        probPtr = probPtr.updated(i, total(currentCity)(nearestNeighborsMatrix(currentCity)(i).get))
        sumProb += probPtr(i)
      }
    })

    if (sumProb <= 0.0){
      /*All cities was visited*/
      chooseBestNext(step, ant)
    } else {
      /*At least one neighbor is eligible, chose one according to the selection probabilities*/
      calculateProb(sumProb, probPtr, step, currentCity, ant)
    }
  }

  def chooseClosestNext(step: Int, ant: Ant): Ant = {
    var nextCity = numberCities
    val currentCity = ant.tour(step - 1)
    var minDistance = Int.MaxValue
    (0 until numberCities).map((city) => {
      if (!ant.visited(city)) {
        if (distance(currentCity.get)(city) < minDistance) {
          nextCity = city
          minDistance = distance(currentCity.get)(city)
        }
      }
    })
    ant.updateTour(step, nextCity)
  }

  def nnTour(): Int = {
    _ants(0).initializeVisited()
    _ants(0).randomInitialPlaceAnt()

    (1 until numberCities).map((step) => {
      _ants= _ants.updated(0,chooseClosestNext(step, _ants(0)))
    })
    _ants(0).tour = _ants(0).tour.updated(numberCities, _ants(0).tour(0))
    if (lsFlagValues.contains(lsFlag)) {
      /*TODO: twoOptFirst()*/
    }
    nTours += 1
    _ants(0).tourLength = computeTourLength(_ants(0).tour)
    val result = _ants(0).tourLength
    _ants(0).initializeVisited()
    result
  }

  def searchControlAndStatistics(nTry: Int): Unit = {
    if (iteration % 100 == 0) {
      branchingFactor = nodeBranching(lambda)
      println("Best so far " + _bestSoFarAnt.tourLength + ", iteration: " + iteration + ", time " + elapsedTime() + ", b_fac " + branchingFactor)
      if (mmasFlag != 0 && (branchingFactor < branchFac) && (iteration - restartFoundBest > 250)) {
        println("INIT TRAILS !!!")
        _restartBestAnt.tourLength = Int.MaxValue
        initPheromoneTrails(trailMax)
        computeTotalInformation()
        restartIteration = iteration
        restartTime = elapsedTime()
      }
      println("try " + nTry + ", iteration " + iteration + ", b-fac " + branchingFactor)
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
    nTours >= maxTours &&
      (elapsedTime() >= maxTime || _bestSoFarAnt.tourLength <= optimal || iteration > maxIterations)
  }

  def updateStatistics(): Unit = {
    var iterationBestAntIter: Integer = null
    iterationBestAntIter = findBest()
    if (_ants(iterationBestAntIter).tourLength < _bestSoFarAnt.tourLength) {
      timeUsed = elapsedTime()
      _ants(iterationBestAntIter).clone(_bestSoFarAnt)
      _ants(iterationBestAntIter).clone(_restartBestAnt)
      foundBest = iteration
      restartFoundBest = iteration
      foundBranching = nodeBranching(lambda)
      branchingFactor = foundBranching
      if (mmasFlag != 0) {
        if (!lsFlagValues.contains(lsFlag)) {
          val px = Math.exp(Math.log(0.05) / numberCities)
          trailMin = 1.0 * (1 - px) / (px * ((nnAnts + 1) / 2))
          trailMax = 1.0 / (rho * _bestSoFarAnt.tourLength)
          trail0 = trailMax
          trailMin = trailMax * trailMin
        } else {
          trailMax = 1.0 / (rho * _bestSoFarAnt.tourLength)
          trailMin = trailMax / (2 * numberCities)
          trail0 = trailMax
        }
      }
    }
    if (_ants(iterationBestAntIter).tourLength < _restartBestAnt.tourLength) {
      _ants(iterationBestAntIter).clone(_restartBestAnt)
      restartFoundBest = iteration
      println("Restart best: " + _restartBestAnt.tourLength + ", restartFoundBest " + restartFoundBest + ", time " + elapsedTime())
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

  def asUpdate(): Unit = {
    _ants.map(ant => globalUpdatePheremone(ant))
  }

  def mmasUpdate(): Unit = {
    if ((iteration % ugb) != 0) {
      val bestAnt = _ants(findBest())
      globalUpdatePheremone(bestAnt)
    } else {
      if (ugb == 1 && (iteration - restartIteration > 50)) globalUpdatePheremone(_bestSoFarAnt) else globalUpdatePheremone(_restartBestAnt)
    }
    ugb = 25
  }

  def pheromoneTrailUpdate(): Unit = {
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
      mmasUpdate()
    }

    if (mmasFlag != 0 && !lsFlagValues.contains(lsFlag)) {
      checkPheromoneTrailLimits()
    }

    if (asFlag != 0 || mmasFlag != 0) {
      computeTotalInformation()
    }

  }


}
