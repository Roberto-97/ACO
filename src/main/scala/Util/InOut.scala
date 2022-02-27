package Util

import Entities.Aco.*
import Entities.ExecutionParameters.*
import Entities.DistanceStrategies.DistanceStrategies
import Entities.DistanceStrategies.{AttDistance, CeilDistance, GeoDistance, RoundDistance}
import Entities.{Aco, Ant, ExecutionParameters, LocalSearch, Tsp}
import Util.Timer.{elapsedTime, startTimer}

import scala.io.Source
import Entities.Tsp.*


object InOut {

  private val NAME_KEY = "NAME:"
  private val DIMENSION_KEY  = "DIMENSION:"
  private val EDGE_WEIGHT_TYPE = "EDGE_WEIGHT_TYPE:"

  private var _nTry: Int = 0
  private var _nTours: Int = 0
  private var _iteration: Int = 0
  private var _restartIteration: Int = 0
  private var _restartTime: Double = 0.0
  private var _lambda: Double = 0.0
  private var _foundBest: Int = 0
  private var _restartFoundBest: Int = 0
  private var _bestInTry: Vector[Option[Int]] = Vector.empty
  private var _bestFoundAt: Vector[Option[Int]] = Vector.empty
  private var _timeBestFound: Vector[Option[Double]] = Vector.empty
  private var _timeTotalRun: Vector[Option[Double]] = Vector.empty
  private var _timeUsed: Double = Double.NaN
  private var _foundBranching: Double = Double.NaN
  private var _branchingFactor: Double = Double.NaN
  private var _timePassed: Double = Double.NaN

  /************************************************* Setters && Getters *******************************************************/

  def nTry = _nTry
  def nTry_=(nTry:Int) = {
    _nTry = nTry
  }

  def nTours = _nTours
  def nTours_=(nTours:Int) = {
    _nTours = nTours
  }

  def iteration = _iteration
  def iteration_=(iteration:Int) = {
    _iteration = iteration
  }

  def restartIteration = _restartIteration
  def restartIteration_=(restartIteration:Int) = {
    _restartIteration = restartIteration
  }

  def restartTime = _restartTime
  def restartTime_=(restartTime:Double) = {
    _restartTime = restartTime
  }

  def lambda = _lambda
  def lambda_=(lambda:Double) = {
    _lambda = lambda
  }

  def foundBest = _foundBest
  def foundBest_=(foundBest:Int) = {
    _foundBest = foundBest
  }

  def restartFoundBest = _restartFoundBest
  def restartFoundBest_=(restartFoundBest:Int) = {
    _restartFoundBest = restartFoundBest
  }

  def bestInTry = _bestInTry
  def bestInTry_=(bestInTry:Vector[Option[Int]]) = {
    _bestInTry = bestInTry
  }

  def bestFoundAt = _bestFoundAt
  def bestFoundAt_=(bestFoundAt:Vector[Option[Int]]) = {
    _bestFoundAt = bestFoundAt
  }

  def timeBestFound = _timeBestFound
  def timeBestFound_=(timeBestFound:Vector[Option[Double]]) = {
    _timeBestFound = timeBestFound
  }

  def timeTotalRun = _timeTotalRun
  def timeTotalRun_=(timeTotalRun:Vector[Option[Double]]) = {
    _timeTotalRun = timeTotalRun
  }

  def timeUsed = _timeUsed
  def timeUsed_=(timeUsed:Double) = {
    _timeUsed = timeUsed
  }

  def foundBranching = _foundBranching
  def foundBranching_=(foundBranching:Double) = {
    _foundBranching = foundBranching
  }

  def branchingFactor = _branchingFactor
  def branchingFactor_=(branchingFactor:Double) = {
    _branchingFactor = branchingFactor
  }

  def timePassed = _timePassed
  def timePassed_=(timePassed:Double) = {
    _timePassed = timePassed
  }


  /****************************************************************************************************************************/

  def initializeParams(): Unit = {
    _bestInTry = Vector.fill(maxTries)(Option.empty)
    _bestFoundAt = Vector.fill(maxTries)(Option.empty)
    _timeBestFound = Vector.fill(maxTries)(Option.empty)
    _timeTotalRun = Vector.fill(maxTries)(Option.empty)
  }


  def selectDistance(edgeWeightType: String): DistanceStrategies = {
    edgeWeightType match {
      case "EUC_2D" => new RoundDistance()
      case "CEIL_2D" => new CeilDistance()
      case "GEO" => new GeoDistance()
      case "ATT" => new AttDistance()
      case _ => new RoundDistance()
    }
  }

  def setNodePtr(lines: Vector[String]): Unit = {
    lines.drop(6)
      .filter(coord => coord != "EOF")
      .map(coord => coord.split(" ").toVector)
      .map(point => setNodeCordSection(point(1).toDouble, point(2).toDouble, point(0).toInt - 1))
  }

  def readEtsp(filename: String): Unit = {
    try {
      println("Read problem data ..")
      val resource = Source.fromResource(filename)
      val lines = resource.getLines().toVector
      val name = lines(0).replaceAll( " ", "").split(NAME_KEY)(1)
      val dimension = lines(3).replaceAll( " ", "").split(DIMENSION_KEY)(1).trim
      val edgeWeightType = lines(4).replaceAll( " ", "").split(EDGE_WEIGHT_TYPE)(1).trim
      initializeTspParams(name, Integer.valueOf(dimension), selectDistance(edgeWeightType))
      setNodePtr(lines)
      println("done ..")
    } catch {
      case x: Exception =>
        println("\nError reading file "+ filename + "...")
    }
  }

  def nodeBranching(lambda: Double): Double = {
    var numBranches = Vector.fill(numberCities)(0.0)
    (0 until numberCities).map((m) => {
      var min = pheremone(m)(nearestNeighborsMatrix(m)(1).get)
      var max = pheremone(m)(nearestNeighborsMatrix(m)(1).get)
      (1 until nnAnts).map(i => {
        if (pheremone(m)(nearestNeighborsMatrix(m)(i).get) > max) {
          max = pheremone(m)(nearestNeighborsMatrix(m)(i).get)
        }
        if (pheremone(m)(nearestNeighborsMatrix(m)(i).get) < min) {
          min = pheremone(m)(nearestNeighborsMatrix(m)(i).get)
        }
      })
      val cutoff = min + lambda * (max - min)
      (0 until nnAnts).map(i => {
        if (pheremone(m)(nearestNeighborsMatrix(m)(i).get) > cutoff)
          numBranches = numBranches.updated(m, numBranches(m) + 1.0)
      })
    })
    val avg = numBranches.reduce((x, y) => x + y)
    avg / (numberCities * 2)
  }

  def initTry(nTry: Int): Unit = {
    println("INITIALIZE TRIAL")
    startTimer()
    _timeUsed = elapsedTime()
    _timePassed = _timeUsed

    /* Initialize variables concerning statistics */
    _nTours = 1
    _iteration = 1
    _restartIteration = 1
    _lambda = 0.05
    bestSoFarAnt.tourLength = Int.MaxValue
    _foundBest = 0

    if (mmasFlag == 0) {
      trail0 = 1.0 / (rho * nnTour())
      initPheromoneTrails(trail0)
    } else {
      trailMax = 1.0 / (rho * nnTour())
      trailMin = trailMax / (2 * numberCities)
      initPheromoneTrails(trailMax)
    }

    /* Calculate combined information pheromone times heuristic information*/
    computeTotalInformation()

    println("Begin try " + nTry + " \n")
  }

  def exitTry(nTry: Int): Unit = {
    checkTour(bestSoFarAnt.tour)
    println("Best Solution in try " + nTry + " is " + bestSoFarAnt.tourLength)
    println("Best Solution was found after " + _foundBest + " iterations")
    _bestInTry = _bestInTry.updated(nTry, Option(bestSoFarAnt.tourLength))
    _bestFoundAt = _bestFoundAt.updated(nTry, Option(_foundBest))
    _timeBestFound = _timeBestFound.updated(nTry, Option(_timeUsed))
    _timeTotalRun = _timeTotalRun.updated(nTry, Option(elapsedTime()))
    println("Try " + nTry + ", Best " + _bestInTry(nTry) + ", found at iteration " + _bestFoundAt(nTry) + ", found at time " + _timeBestFound(nTry))
    println("End try")
  }

  def exitProgram(): Unit = {
    val bestTourLength = _bestInTry.minBy(_.get)
    val worstTourLength = _bestInTry.maxBy(_.get)
    println("Best try: " + bestTourLength + " Worst try: " + worstTourLength)
    println("End problem " + name)
  }

  def checkTour(tour: Vector[Option[Integer]]): Unit = {
    val vectSum: Vector[Integer] = tour.map(e => e.getOrElse(0))
    val sum: Int = vectSum.dropRight(1).reduce((x,y) => x + y)
    if (sum != ((numberCities - 1) * numberCities)/ 2) {
      println("Next tour must be flawed !!")
      printTour(tour)
      println("Tour sum: " + sum)
    }
  }

  def printTour(tour: Vector[Option[Integer]]): Unit = {
    tour.map(city => println(""+ city))
    println("Tour Length = " + computeTourLength(tour))
  }

  def printParameters(): Unit = {
    println("\nExecution parameters:")
    println("maxTries -> " + maxTries)
    println("maxTours -> " + maxTours)
    println("maxTime -> " + maxTime)
    println("optimum -> " + optimal)
    println("nAnts -> " + nAnts)
    println("nnAnts -> " + nnAnts)
    println("alpha -> " + alpha)
    println("beta -> " + beta)
    println("rho -> " + rho)
    println("q0 -> " + q0)
    println("elitistAnts -> " + elitistsAnts)
    println("rasRanks -> " + rasRanks)
    println("lsFlag -> " + lsFlag)
    println("nnLs -> " + nnLs)
    println("asFlag -> " + asFlag)
    println("mmasFlag -> " + mmasFlag)
    println("\n")
  }

  def initProgram(): Unit = {
    initializeParams()
    readEtsp(tsplibfile)
    if (nAnts < 0)
      nAnts = numberCities

    nnLs = (numberCities - 1).min(nnLs)

    allocateAnts()

    println("Calculating distance matrix ..")
    computeDistances()
    println("done ..")
    printParameters()
  }
}
