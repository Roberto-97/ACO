package Util

import Entities.DistanceStrategies.DistanceStrategies
import Entities.DistanceStrategies.{AttDistance, CeilDistance, GeoDistance, RoundDistance}
import Entities.{Aco, ExecutionParameters, LocalSearch, Tsp, Ant}

import scala.io.Source
import Entities.Tsp.{computeDistances, initializeTspParams, numberCities, setNodeCordSection}

object InOut {

  private val NAME_KEY = "NAME:"
  private val DIMENSION_KEY  = "DIMENSION:"
  private val EDGE_WEIGHT_TYPE = "EDGE_WEIGHT_TYPE:"
  private var _nTry: Int = 0
  var _nTours: Int = 0
  private var _iteration: Int = 0
  private var _restartIteration: Int = 0
  private var _restartTime: Double = 0.0
  private var _maxTries: Int = 0
  private var _maxTours: Int = 0
  private var _lambda: Double = 0.0
  private var _foundBest: Int = 0

  def initTry(): Unit = {
    _nTours = 1
    _iteration = 1
    _restartIteration = 1
    _lambda = 0.05
    Aco._bestSoFarAnt.tourLength = Int.MaxValue
    _foundBest = 0
    if (!(ExecutionParameters.acsFlag != 0  || ExecutionParameters.mmasFlag != 0 || ExecutionParameters.bwasFlag != 0)) {
      ExecutionParameters.trail0 = 1. / (ExecutionParameters.rho * Aco.nnTour())
    }
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
      val resource = Source.fromResource(filename)
      val lines = resource.getLines().toVector
      val name = lines(0).split(NAME_KEY)(1).trim
      val dimension = lines(3).split(DIMENSION_KEY)(1).trim
      val edgeWeightType = lines(4).split(EDGE_WEIGHT_TYPE)(1).trim
      initializeTspParams(name, Integer.valueOf(dimension), selectDistance(edgeWeightType))
      setNodePtr(lines)
    } catch {
      case x: Exception =>
        println("\nError reading file "+ filename + "...")
    }
  }


  def printParameters(): Unit = {
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
    println("dlbFlag -> " + ExecutionParameters.dlbFlag)
    println("asFlag -> " + ExecutionParameters.asFlag)
    println("easFlag -> " + ExecutionParameters.easFlag)
    println("rasFlag -> " + ExecutionParameters.rasFlag)
    println("mmasFlag -> " + ExecutionParameters.mmasFlag)
    println("bwasFlag -> " + ExecutionParameters.bwasFlag)
    println("acsFlag -> " + ExecutionParameters.acsFlag)
  }

  def initProgram(): Unit = {
    readEtsp(ExecutionParameters.tsplibfile)
    if (ExecutionParameters.nAnts < 0)
      ExecutionParameters.nAnts = numberCities
    if (ExecutionParameters.easFlag != 0 && ExecutionParameters.elitistsAnts <= 0)
      ExecutionParameters.elitistsAnts = numberCities
    ExecutionParameters.nnLs = (numberCities - 1).min(ExecutionParameters.nnLs)

    Aco.init(ExecutionParameters.nAnts)

    println("\nCalculating distance matrix .. \n\n")
    computeDistances()
    println("\n..done\n")
    printParameters()
  }
}
