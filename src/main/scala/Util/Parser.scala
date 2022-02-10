package Util

import Entities.DistanceStrategies.DistanceStrategies
import Entities.DistanceStrategies.{AttDistance, CeilDistance, GeoDistance, RoundDistance}
import Entities.ExecutionParameters

import scala.io.Source
import Entities.Tsp.{computeDistances, initializeTspParams, numberCities, setNodeCordSection}

object Parser {

  private val NAME_KEY = "NAME:"
  private val DIMENSION_KEY  = "DIMENSION:"
  private val EDGE_WEIGHT_TYPE = "EDGE_WEIGHT_TYPE:"

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


  def printParameters(ep: ExecutionParameters): Unit = {
    println("maxTries -> " + ep.maxTries)
    println("maxTours -> " + ep.maxTours)
    println("maxTime -> " + ep.maxTime)
    println("optimum -> " + ep.optimal)
    println("nAnts -> " + ep.nAnts)
    println("nnAnts -> " + ep.nnAnts)
    println("alpha -> " + ep.alpha)
    println("beta -> " + ep.beta)
    println("rho -> " + ep.rho)
    println("q0 -> " + ep.q0)
    println("elitistAnts -> " + ep.elitistRanks)
    println("rasRanks -> " + ep.rasRanks)
    println("lsFlag -> " + ep.lsFlag)
    println("nnLs -> " + ep.nnLs)
    println("dlbFlag -> " + ep.dlbFlag)
    println("asFlag -> " + ep.asFlag)
    println("easFlag -> " + ep.easFlag)
    println("rasFlag -> " + ep.rasFlag)
    println("mmasFlag -> " + ep.mmasFlag)
    println("bwasFlag -> " + ep.bwasFlag)
    println("acsFlag -> " + ep.acsFlag)
  }

  def initProgram(ep: ExecutionParameters): Unit = {
    readEtsp(ep.tsplibfile)
    if (ep.nAnts < 0)
      ep.nAnts = numberCities
    if (ep.easFlag != 0 && ep.elitistRanks <= 0)
      ep.elitistRanks = numberCities
    ep.nnLs = (numberCities - 1).min(ep.nnLs)

    println("\nCalculating distance matrix .. \n\n")
    computeDistances()
    println("\n..done\n")
    printParameters(ep)
  }
}
