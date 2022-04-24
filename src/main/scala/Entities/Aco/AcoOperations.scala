package Entities.Aco

import Entities.ExecutionParameters._
import Entities.Tsp._
import Entities.{Ant, Colonie}
import Util.InOut._
import Util.SparkConf.getSparkContext
import Util.Timer.elapsedTime
import org.apache.spark.TaskContext

import scala.util.Random

object AcoOperations {

  def calculateProb(sumProb: Double, probPtr: Vector[Double], step: Int, currentCity: Integer, ant: Ant, colonie: Colonie): Ant = {
    var random = randomNumber.nextDouble()
    random *= sumProb
    var i = 0
    var partialSum = probPtr(i)
    while (partialSum <= random) {
      i += 1
      partialSum += probPtr(i)
    }

    if (i == nnAnts) {
      return neighbourChooseBestNext(step, ant, colonie)
    }
    val help = nearestNeighborsMatrix(currentCity)(i).get
    ant.updateTour(step, help)
  }

  /*
  * chooses for an ant as the next city the one with
  * maximal value of heuristic information times pheromone
  * */
  def chooseBestNext(step: Int, ant: Ant, colonie: Colonie): Ant = {
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

  /*
  * chooses for an ant as the next city the one with
  * maximal value of heuristic information times pheromone
  */
  def neighbourChooseBestNext(step: Int, ant: Ant, colonie: Colonie): Ant = {
    var nextCity = numberCities
    val currentCity = ant.tour(step - 1).get
    var valueBest = -1.0
    (0 until nnAnts).map((i) => {
      val helpCity = nearestNeighborsMatrix(currentCity)(i).get
      if (!ant.visited(helpCity)) {
        val help = colonie.total(currentCity)(helpCity)
        if (help > valueBest) {
          valueBest = help
          nextCity = helpCity
        }
      }
    })

    if (nextCity == numberCities) {
      chooseBestNext(step, ant, colonie)
    } else {
      ant.updateTour(step, nextCity)
    }
  }

  /*
  * Choose for an ant probabilistically a next city among all unvisited cities
  * in the current city's candidate list. If this is not possible, choose the closest next
  * */
  def neighbourChooseAndMoveToNext(step: Int, ant: Ant, colonie: Colonie): Ant = {
    var sumProb = 0.0
    var probPtr = Vector.fill(nnAnts + 1)(0.0).updated(nnAnts, Double.MaxValue)

    if ((q0 > 0.0) && (randomNumber.nextDouble() < q0)) {
      /* with a probability q0 make the best possible choice according to pheremone trails and heuristic information*/
      return neighbourChooseBestNext(step, ant, colonie)
    }

    val currentCity = ant.tour(step - 1).get
    (0 until nnAnts).map((i) => {
      if (ant.visited(nearestNeighborsMatrix(currentCity)(i).get)) {
        probPtr = probPtr.updated(i, 0.0) /* City already visited */
      } else {
        probPtr = probPtr.updated(i, colonie.total(currentCity)(nearestNeighborsMatrix(currentCity)(i).get))
        sumProb += probPtr(i)
      }
    })

    if (sumProb <= 0.0) {
      /*All cities was visited*/
      chooseBestNext(step, ant, colonie)
    } else {
      /*At least one neighbor is eligible, chose one according to the selection probabilities*/
      calculateProb(sumProb, probPtr, step, currentCity, ant, colonie)
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

  def evaluateAntsMasterSlave(colonie: Colonie): Unit = {
    colonie.ants = getSparkContext().parallelize(colonie.ants).mapPartitions(iterator => {
      val tc = TaskContext.get()
      randomNumber = new Random(seed * (tc.partitionId() + 1))
      iterator.map(ant => {
        /* Mark all cities as unvisited*/
        ant.initializeVisited()
        /* Place the ants on same initial city*/
        ant.randomInitialPlaceAnt()
        /* Choose the nexts cities to visit*/
        (1 until numberCities).map((step) => neighbourChooseAndMoveToNext(step, ant, colonie))
        /* Compute tour length*/
        ant.computeTour()
      })
    }).collect().toVector
  }

  def constructSolutions(colonie: Colonie): Unit = {
    this.evaluateAnts(colonie)
    nTours += nAnts
  }

  def constructSolutionsMasterSlave(colonie: Colonie): Unit = {
    this.evaluateAntsMasterSlave(colonie)
    nTours += nAnts
  }

  def searchControlAndStatistics(nTry: Int, colonie: Colonie): Unit = {
    if (iteration % 100 == 0) {
      branchingFactor = nodeBranching(lambda, colonie)
      println("Best so far " + colonie.bestSoFarAnt.tourLength + ", iteration: " + iteration + ", time " + elapsedTime() + ", b_fac " + branchingFactor)
      if (mmasFlag != 0 && (branchingFactor < branchFac) && (iteration - restartFoundBest > 250)) {
        println("INIT TRAILS !!!")
        colonie.restartBestAnt.tourLength = Int.MaxValue
        colonie.initPheromoneTrails(trailMax)
        colonie.computeTotalInformation()
        restartIteration = iteration
        restartTime = elapsedTime()
      }
      println("try " + nTry + ", iteration " + iteration + ", b-fac " + branchingFactor)
    }
  }

  def terminationCondition(colonie: Colonie): Boolean = {
    nTours >= maxTours &&
      (elapsedTime() >= maxTime || colonie.bestSoFarAnt.tourLength <= optimal || iteration > maxIterations)
  }

  def nnTour(colonie: Colonie): Int = {
    colonie.ants(0).initializeVisited()
    colonie.ants(0).randomInitialPlaceAnt()

    (1 until numberCities).map((step) => {
      colonie.ants = colonie.ants.updated(0, chooseClosestNext(step, colonie.ants(0)))
    })
    colonie.ants(0).tour = colonie.ants(0).tour.updated(numberCities, colonie.ants(0).tour(0))
    if (lsFlagValues.contains(lsFlag)) {
      /*TODO: twoOptFirst()*/
    }
    nTours += 1
    colonie.ants(0).tourLength = computeTourLength(colonie.ants(0).tour)
    val result = colonie.ants(0).tourLength
    colonie.ants(0).initializeVisited()
    result
  }

  def updateCommonStats(colonie: Colonie): Unit = {
    timeUsed = elapsedTime()
    foundBest = iteration
    restartFoundBest = iteration
    foundBranching = nodeBranching(lambda, colonie)
    branchingFactor = foundBranching
    if (mmasFlag != 0) {
      if (!lsFlagValues.contains(lsFlag)) {
        val px = Math.exp(Math.log(0.05) / numberCities)
        trailMin = 1.0 * (1 - px) / (px * ((nnAnts + 1) / 2))
        trailMax = 1.0 / (rho * colonie.bestSoFarAnt.tourLength)
        trail0 = trailMax
        trailMin = trailMax * trailMin
      } else {
        trailMax = 1.0 / (rho * colonie.bestSoFarAnt.tourLength)
        trailMin = trailMax / (2 * numberCities)
        trail0 = trailMax
      }
    }
  }

  def updateStatistics(colonie: Colonie): Unit = {
    var iterationBestAntIter: Integer = null
    iterationBestAntIter = colonie.findBest()
    if (colonie.ants(iterationBestAntIter).tourLength < colonie.bestSoFarAnt.tourLength) {
      colonie.ants(iterationBestAntIter).clone(colonie.bestSoFarAnt)
      colonie.ants(iterationBestAntIter).clone(colonie.restartBestAnt)
      updateCommonStats(colonie)
    }
    if (colonie.ants(iterationBestAntIter).tourLength < colonie.restartBestAnt.tourLength) {
      colonie.ants(iterationBestAntIter).clone(colonie.restartBestAnt)
      restartFoundBest = iteration
      println("Restart best: " + colonie.restartBestAnt.tourLength + ", restartFoundBest " + restartFoundBest + ", time " + elapsedTime())
    }
  }

  def pheromoneTrailUpdate(colonie: Colonie): Unit = {
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
      colonie.mmasUpdate()
    }

    if (mmasFlag != 0 && !lsFlagValues.contains(lsFlag)) {
      colonie.checkPheromoneTrailLimits()
    }

    if (asFlag != 0 || mmasFlag != 0) {
      colonie.computeTotalInformation()
    }

  }
  
  def searchControlAndStatisticsColonie(nTry: Int, colonie: Colonie): Unit = {
    branchingFactor = nodeBranching(lambda, colonie)
    println("INIT TRAILS !!!")
    if (mmasFlag != 0 && (branchingFactor < branchFac)) {
      colonie.restartBestAnt.tourLength = Int.MaxValue
      colonie.initPheromoneTrails(trailMax)
      colonie.computeTotalInformation()
      restartIteration = iteration
      restartTime = elapsedTime()
    }
    println("try " + nTry + ", iteration " + iteration + ", b-fac " + branchingFactor)
  }

}
