package Util

import Entities.Aco.{_bestSoFarAnt, computeTotalInformation, initPheromoneTrails, nnTour}
import Entities.DistanceStrategies.DistanceStrategies
import Entities.DistanceStrategies.{AttDistance, CeilDistance, GeoDistance, RoundDistance}
import Entities.{Aco, Ant, ExecutionParameters, LocalSearch, Tsp}
import Util.Timer.{startTimer, elapsedTime}

import scala.io.Source
import Entities.Tsp.{computeDistances, initializeTspParams, nearestNeighborsMatrix, numberCities, setNodeCordSection}

object InOut {

  private val NAME_KEY = "NAME:"
  private val DIMENSION_KEY  = "DIMENSION:"
  private val EDGE_WEIGHT_TYPE = "EDGE_WEIGHT_TYPE:"

  private var _nTry: Int = 0
  var _nTours: Int = 0
  var _iteration: Int = 0
  var _restartIteration: Int = 0
  var _restartTime: Double = 0.0
  private var _maxTries: Int = 0
  private var _maxTours: Int = 0
  var _lambda: Double = 0.0
  private var _foundBest: Int = 0
  private var _bestInTry: Vector[Option[Int]] = Vector.empty
  private var _bestFoundAt: Vector[Option[Int]] = Vector.empty
  private var _timeBestFound: Vector[Option[Double]] = Vector.empty
  private var _timeTotalRun: Vector[Option[Double]] = Vector.empty

  def initializeParams(): Unit = {
    _bestInTry = Vector.fill(ExecutionParameters.maxTries)(Option.empty)
    _bestFoundAt = Vector.fill(ExecutionParameters.maxTries)(Option.empty)
    _timeBestFound = Vector.fill(ExecutionParameters.maxTries)(Option.empty)
    _timeTotalRun = Vector.fill(ExecutionParameters.maxTries)(Option.empty)
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
      val name = lines(0).split(NAME_KEY)(1).trim
      val dimension = lines(3).split(DIMENSION_KEY)(1).trim
      val edgeWeightType = lines(4).split(EDGE_WEIGHT_TYPE)(1).trim
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
      var min = Aco.pheremone(m)(nearestNeighborsMatrix(m)(1).get)
      var max = Aco.pheremone(m)(nearestNeighborsMatrix(m)(1).get)
      (0 until ExecutionParameters.nnAnts).map(i => {
        if (Aco.pheremone(m)(nearestNeighborsMatrix(m)(i).get) > max) {
          max = Aco.pheremone(m)(nearestNeighborsMatrix(m)(i).get)
        }
        if (Aco.pheremone(m)(nearestNeighborsMatrix(m)(i).get) < min) {
          min = Aco.pheremone(m)(nearestNeighborsMatrix(m)(i).get)
        }
      })
      val cutoff = min + lambda * (max - min)
      (0 until ExecutionParameters.nnAnts).map(i => {
        if (Aco.pheremone(m)(nearestNeighborsMatrix(m)(i).get) > cutoff)
          numBranches = numBranches.updated(m, numBranches(m) + 1.0)
      })
    })
    val avg = numBranches.reduce((x, y) => x + y)
    avg / (numberCities * 2)
  }

  def initTry(nTry: Int): Unit = {
    println("INITIALIZE TRIAL")
    startTimer()
    val timeUsed = elapsedTime()
    val timePassed = timeUsed

    /* Initialize variables concerning statistics */
    _nTours = 1
    _iteration = 1
    _restartIteration = 1
    _lambda = 0.05
    _bestSoFarAnt.tourLength = Int.MaxValue
    _foundBest = 0

    if (ExecutionParameters.mmasFlag == 0) {
      ExecutionParameters.trail0 = 1.0 / (ExecutionParameters.rho * nnTour())
      initPheromoneTrails(ExecutionParameters.trail0)
    } else {
      ExecutionParameters.trailMax = 1.0 / (ExecutionParameters.rho * nnTour())
      ExecutionParameters.trailMin = ExecutionParameters.trailMax / (2 * numberCities)
      initPheromoneTrails(ExecutionParameters.trailMax)
    }

    /* Calculate combined information pheromone times heuristic information*/
    computeTotalInformation()

    println("Begin try " + nTry + " \n")
  }

  def exitTry(nTry: Int): Unit = {
    checkTour(Aco._bestSoFarAnt.tour)
    println("Best solution in try is " + Aco._bestSoFarAnt.tourLength)
    _bestInTry = _bestInTry.updated(nTry, Option(Aco._bestSoFarAnt.tourLength))

  }

  def checkTour(tour: Vector[Option[Integer]]): Unit = {
    val sum: Int = tour.map(e => e.get).reduce((x,y) => x + y)
    if (sum != ((numberCities - 1) * 2)/ 2) {
      println("Next tour must be flawed !!")
    }
  }

  def printParameters(): Unit = {
    println("\nExecution parameters:")
    println("maxTries -> " + ExecutionParameters.maxTries)
    println("maxTours -> " + ExecutionParameters.maxTours)
    println("maxTime -> " + ExecutionParameters.maxTime)
    println("optimum -> " + ExecutionParameters.optimal)
    println("nAnts -> " + ExecutionParameters.nAnts)
    println("nnAnts -> " + ExecutionParameters.nnAnts)
    println("alpha -> " + ExecutionParameters.alpha)
    println("beta -> " + ExecutionParameters.beta)
    println("rho -> " + ExecutionParameters.rho)
    println("q0 -> " + ExecutionParameters.q0)
    println("elitistAnts -> " + ExecutionParameters.elitistsAnts)
    println("rasRanks -> " + ExecutionParameters.rasRanks)
    println("lsFlag -> " + ExecutionParameters.lsFlag)
    println("nnLs -> " + ExecutionParameters.nnLs)
    println("asFlag -> " + ExecutionParameters.asFlag)
    println("mmasFlag -> " + ExecutionParameters.mmasFlag)
    println("\n")
  }

  def initProgram(): Unit = {
    initializeParams()
    readEtsp(ExecutionParameters.tsplibfile)
    if (ExecutionParameters.nAnts < 0)
      ExecutionParameters.nAnts = numberCities

    ExecutionParameters.nnLs = (numberCities - 1).min(ExecutionParameters.nnLs)

    Aco.allocateAnts()

    println("Calculating distance matrix ..")
    computeDistances()
    println("done ..")
    printParameters()
  }
}