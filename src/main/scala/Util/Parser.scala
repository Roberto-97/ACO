package Util

import Entities.DistanceStrategies.DistanceStrategies
import Entities.DistanceStrategies.{ RoundDistance, CeilDistance, GeoDistance, AttDistance }

import scala.io.Source
import Entities.Tsp.{ initializeTspParams, setNodeCordSection }

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
        println("\n Error reading file "+ filename + "...")
    }
  }
}
