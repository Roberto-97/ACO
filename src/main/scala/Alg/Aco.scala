package Alg

import Entities.Tsp.{computeDistances, computeTourLength, nodeBranching}
import Entities._
import Util.InOut._
import Util.Timer.{elapsedTime, startTimer}
import org.apache.spark.SparkContext

abstract class Aco {

  def evaluateAnts(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    colonie.ants = colonie.ants.map(ant => {
      /* Mark all cities as unvisited*/
      ant.initializeVisited(tspParameters.numberCities)
      /* Place the ants on same initial city*/
      ant.randomInitialPlaceAnt(tspParameters.numberCities, tspParameters.randomNumber)
      /* Choose the nexts cities to visit*/
      (1 until tspParameters.numberCities).map((step) => neighbourChooseAndMoveToNext(step, ant, colonie, ep, tspParameters))
      /* Compute tour length*/
      ant.computeTour(tspParameters.numberCities, tspParameters.distance)
    })
  }

  def constructSolutions(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    this.evaluateAnts(colonie, ep, tspParameters)
    tspParameters.nTours += ep.nAnts
  }

  def updateCommonStats(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    colonie.timeUsed = elapsedTime()
    colonie.foundBest = tspParameters.iteration
    colonie.restartFoundBest = tspParameters.iteration
    colonie.foundBranching = nodeBranching(colonie, ep, tspParameters)
    colonie.branchingFactor = colonie.foundBranching
    if (ep.mmasFlag != 0) {
      if (!Vector(1, 2, 3).contains(ep.lsFlag)) {
        val px = Math.exp(Math.log(0.05) / tspParameters.numberCities)
        ep.trailMin = 1.0 * (1 - px) / (px * ((ep.nnAnts + 1) / 2))
        ep.trailMax = 1.0 / (ep.rho * colonie.bestSoFarAnt.tourLength.get)
        ep.trail0 = ep.trailMax
        ep.trailMin = ep.trailMax * ep.trailMin
      } else {
        ep.trailMax = 1.0 / (ep.rho * colonie.bestSoFarAnt.tourLength.get)
        ep.trailMin = ep.trailMax / (2 * tspParameters.numberCities)
        ep.trail0 = ep.trailMax
      }
    }
  }

  def updateStatistics(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    var iterationBestAntIter: Option[Int] = None
    iterationBestAntIter = Option(colonie.findBest())
    if (colonie.ants(iterationBestAntIter.get).tourLength.get < colonie.bestSoFarAnt.tourLength.get) {
      colonie.ants(iterationBestAntIter.get).clone(colonie.bestSoFarAnt)
      colonie.ants(iterationBestAntIter.get).clone(colonie.restartBestAnt)
      updateCommonStats(colonie, ep, tspParameters)
    }
    if (colonie.ants(iterationBestAntIter.get).tourLength.get < colonie.restartBestAnt.tourLength.get) {
      colonie.ants(iterationBestAntIter.get).clone(colonie.restartBestAnt)
      colonie.restartFoundBest = tspParameters.iteration
      println("Restart best: " + colonie.restartBestAnt.tourLength + ", restartFoundBest " + colonie.restartFoundBest + ", time " + elapsedTime())
    }
  }

  def pheromoneTrailUpdate(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    if (ep.asFlag != 0 || ep.mmasFlag != 0) {
      if (Vector(1, 2, 3).contains(ep.lsFlag)) {
        /*TODO: LocalSearch*/
      } else {
        colonie.evaporation(ep, tspParameters.numberCities)
      }
    }

    if (ep.asFlag != 0) {
      colonie.asUpdate(tspParameters.numberCities)
    } else if (ep.mmasFlag != 0) {
      colonie.mmasUpdate(ep, tspParameters)
    }

    if (ep.mmasFlag != 0 && !Vector(1, 2, 3).contains(ep.lsFlag)) {
      colonie.checkPheromoneTrailLimits(ep, tspParameters.numberCities)
    }

    if (ep.asFlag != 0 || ep.mmasFlag != 0) {
      colonie.computeTotalInformation(ep, tspParameters.numberCities, tspParameters.distance)
    }

  }

  def searchControlAndStatistics(nTry: Int, colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    if (tspParameters.iteration % 100 == 0) {
      colonie.branchingFactor = nodeBranching(colonie, ep, tspParameters)
      println("Best so far " + colonie.bestSoFarAnt.tourLength + ", iteration: " + tspParameters.iteration +
        ", time " + elapsedTime() + ", b_fac " + colonie.branchingFactor)
      if (ep.mmasFlag != 0 && (colonie.branchingFactor < ep.branchFac) && (tspParameters.iteration - colonie.restartFoundBest > 250)) {
        println("INIT TRAILS !!!")
        colonie.restartBestAnt.tourLength = Option(Int.MaxValue)
        colonie.initPheromoneTrails(ep.trailMax, tspParameters.numberCities)
        colonie.computeTotalInformation(ep, tspParameters.numberCities, tspParameters.distance)
        colonie.restartIteration = tspParameters.iteration
      }
      println("try " + nTry + ", iteration " + tspParameters.iteration)
    }
  }


  def terminationCondition(colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Boolean = {
    (elapsedTime() >= ep.maxTime || colonie.bestSoFarAnt.tourLength.get <= ep.optimal || tspParameters.iteration > ep.maxIterations)
  }

  def chooseClosestNext(step: Int, ant: Ant, numberCities: Int, distance: Vector[Vector[Int]]): Ant = {
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

  def neighbourChooseAndMoveToNext(step: Int, ant: Ant, colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Ant = {
    var sumProb = 0.0
    var probPtr = Vector.fill(ep.nnAnts + 1)(0.0).updated(ep.nnAnts, Double.MaxValue)

    if ((ep.q0 > 0.0) && (tspParameters.randomNumber.nextDouble() < ep.q0)) {
      /* with a probability q0 make the best possible choice according to pheremone trails and heuristic information*/
      return neighbourChooseBestNext(step, ant, colonie, ep, tspParameters)
    }

    val currentCity = ant.tour(step - 1).get
    (0 until ep.nnAnts).map((i) => {
      if (ant.visited(tspParameters.nearestNeighborsMatrix(currentCity)(i).get)) {
        probPtr = probPtr.updated(i, 0.0) /* City already visited */
      } else {
        probPtr = probPtr.updated(i, colonie.total(currentCity)(tspParameters.nearestNeighborsMatrix(currentCity)(i).get))
        sumProb += probPtr(i)
      }
    })

    if (sumProb <= 0.0) {
      /*All cities was visited*/
      chooseBestNext(step, ant, colonie, tspParameters.numberCities)
    } else {
      /*At least one neighbor is eligible, chose one according to the selection probabilities*/
      calculateProb(sumProb, probPtr, step, currentCity, ant, colonie, ep, tspParameters)
    }
  }

  def chooseBestNext(step: Int, ant: Ant, colonie: Colonie, numberCities: Int): Ant = {
    var nextCity = numberCities
    val currentCity = ant.tour(step - 1).get
    var valueBest = -1.0
    (0 until numberCities).map((city) => {
      if (!ant.visited(city)) {
        if (colonie.total(currentCity)(city) > valueBest) {
          nextCity = city
          valueBest = colonie.total(currentCity)(city)
        }
      }
    })
    ant.updateTour(step, nextCity)
  }

  def calculateProb(sumProb: Double, probPtr: Vector[Double], step: Int, currentCity: Int, ant: Ant, colonie: Colonie, ep: ExecutionParameters,
                    tspParameters: TspParameters): Ant = {
    var random = tspParameters.randomNumber.nextDouble()
    random *= sumProb
    var i = 0
    var partialSum = probPtr(i)
    while (partialSum <= random) {
      i += 1
      partialSum += probPtr(i)
    }

    if (i == ep.nnAnts) {
      return neighbourChooseBestNext(step, ant, colonie, ep, tspParameters)
    }
    val help = tspParameters.nearestNeighborsMatrix(currentCity)(i).get
    ant.updateTour(step, help)
  }

  def neighbourChooseBestNext(step: Int, ant: Ant, colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Ant = {
    var nextCity = tspParameters.numberCities
    val currentCity = ant.tour(step - 1).get
    var valueBest = -1.0
    (0 until ep.nnAnts).map((i) => {
      val helpCity = tspParameters.nearestNeighborsMatrix(currentCity)(i).get
      if (!ant.visited(helpCity)) {
        val help = colonie.total(currentCity)(helpCity)
        if (help > valueBest) {
          valueBest = help
          nextCity = helpCity
        }
      }
    })

    if (nextCity == tspParameters.numberCities) {
      chooseBestNext(step, ant, colonie, tspParameters.numberCities)
    } else {
      ant.updateTour(step, nextCity)
    }
  }

  def nnTour(colonie: Colonie, tspParameters: TspParameters): Int = {
    colonie.ants(0).initializeVisited(tspParameters.numberCities)
    colonie.ants(0).randomInitialPlaceAnt(tspParameters.numberCities, tspParameters.randomNumber)

    (1 until tspParameters.numberCities).map((step) => {
      colonie.ants = colonie.ants.updated(0, chooseClosestNext(step, colonie.ants(0), tspParameters.numberCities, tspParameters.distance))
    })
    colonie.ants(0).tour = colonie.ants(0).tour.updated(tspParameters.numberCities, colonie.ants(0).tour(0))
    tspParameters.nTours += 1
    colonie.ants(0).tourLength = Option(computeTourLength(colonie.ants(0).tour, tspParameters.numberCities, tspParameters.distance))
    val result = colonie.ants(0).tourLength
    colonie.ants(0).initializeVisited(tspParameters.numberCities)
    result.get
  }

  def initTry(ep: ExecutionParameters, tspParameters: TspParameters, colonies: Vector[Colonie]): Unit = {
    println("INITIALIZE TRIAL")
    startTimer()

    /* Initialize variables concerning statistics */
    tspParameters.nTours = 1
    tspParameters.iteration = 1
    tspParameters.lambda = 0.05
    colonies.map(colonie => {
      colonie.bestSoFarAnt.tourLength = Option(Int.MaxValue)
      colonie.timeUsed = elapsedTime()
      colonie.restartIteration = 1
    })

    if (ep.mmasFlag == 0) {
      colonies.map(colonie => {
        ep.trail0 = 1.0 / (ep.rho * nnTour(colonie, tspParameters))
        colonie.initPheromoneTrails(ep.trail0, tspParameters.numberCities)
      })
    } else {
      colonies.map(colonie => {
        ep.trailMax = 1.0 / (ep.rho * nnTour(colonie, tspParameters))
        ep.trailMin = ep.trailMax / (2 * tspParameters.numberCities)
        colonie.initPheromoneTrails(ep.trailMax, tspParameters.numberCities)
      })
    }

    /* Calculate combined information pheromone times heuristic information*/
    colonies.map(colonie => colonie.computeTotalInformation(ep, tspParameters.numberCities, tspParameters.distance))
  }

  def run(ep: ExecutionParameters, sparkContext: Option[SparkContext]): Unit = {
    startTimer()
    val tspParameters = readEtsp(ep.tsplibfile, ep.seed)
    if (ep.nAnts < 0)
      ep.nAnts = tspParameters.numberCities
    initializeResults(ep)
    ep.nnLs = (tspParameters.numberCities - 1).min(ep.nnLs)
    println("Calculating distance matrix ..")
    tspParameters.distance = computeDistances(tspParameters.numberCities, tspParameters.distanceStrategy, tspParameters.nodePtr)
    println("done ..")
    printParameters(ep)
    Tsp.computeNearestNeighborsMatrix(ep, tspParameters)
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    val colonie: Colonie = new Colonie().initializeColonie(ep, tspParameters.numberCities)
    (0 until ep.maxTries).map(nTry => {
      println("Begin try " + nTry + " \n")
      initTry(ep, tspParameters, Vector(colonie))
      while (!terminationCondition(colonie, ep, tspParameters)) {
        constructSolutions(colonie, ep, tspParameters)
        updateStatistics(colonie, ep, tspParameters)
        pheromoneTrailUpdate(colonie, ep, tspParameters)
        searchControlAndStatistics(nTry, colonie, ep, tspParameters)
        tspParameters.iteration += 1
      }
      exitTry(nTry, colonie, tspParameters)
    })
    writeReport(sparkContext, tspParameters.name, ep)
  }
}
