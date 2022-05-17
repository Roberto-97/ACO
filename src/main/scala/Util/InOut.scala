package Util

import Entities.DistanceStrategies._
import Entities.{Colonie, ExecutionParameters, Point, TspParameters}
import Util.Timer.elapsedTime
import org.apache.spark.SparkContext

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.util.Random

object InOut {

  private val NAME_KEY = "NAME:"
  private val DIMENSION_KEY = "DIMENSION:"
  private val EDGE_WEIGHT_TYPE = "EDGE_WEIGHT_TYPE:"

  private var _bestInTry: Vector[Option[Int]] = Vector.empty
  private var _bestFoundAt: Vector[Option[Int]] = Vector.empty
  private var _timeBestFound: Vector[Option[Double]] = Vector.empty
  private var _timeTotalRun: Vector[Option[Double]] = Vector.empty

  def initializeResults(ep: ExecutionParameters): Unit = {
    _bestInTry = Vector.fill(ep.maxTries)(Option.empty)
    _bestFoundAt = Vector.fill(ep.maxTries)(Option.empty)
    _timeBestFound = Vector.fill(ep.maxTries)(Option.empty)
    _timeTotalRun = Vector.fill(ep.maxTries)(Option.empty)
  }

  def exitTry(nTry: Int, colonie: Colonie, tspParameters: TspParameters): Unit = {
    checkTour(colonie.bestSoFarAnt.tour, tspParameters.numberCities)
    println("Best Solution in try " + nTry + " is " + colonie.bestSoFarAnt.tourLength)
    println("Best Solution was found after " + colonie.foundBest + " iterations")
    _bestInTry = _bestInTry.updated(nTry, Option(colonie.bestSoFarAnt.tourLength.get))
    _bestFoundAt = _bestFoundAt.updated(nTry, Option(colonie.foundBest))
    _timeBestFound = _timeBestFound.updated(nTry, Option(colonie.timeUsed))
    _timeTotalRun = _timeTotalRun.updated(nTry, Option(elapsedTime()))
    println("Try " + nTry + ", Best " + _bestInTry(nTry) + ", found at iteration " + _bestFoundAt(nTry) + ", found at time " + _timeBestFound(nTry))
    println("End try")
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

  def setNodePtr(lines: Vector[String], numberCities: Int): Vector[Point] = {
    var nodePtr: Vector[Point] = Vector.fill(numberCities)(null)
    lines.drop(6)
      .filter(coord => coord != "EOF")
      .map(coord => coord.split(" ").toVector)
      .foreach(point => {
        nodePtr = nodePtr.updated((point(0).toInt - 1), Point(point(1).toDouble, point(2).toDouble))
      })
    nodePtr
  }

  def readEtsp(filename: String, seed: Long): TspParameters = {
    try {
      println("Read problem data ..")
      val iStream = getClass.getResourceAsStream("/" + filename)
      val resource = Source.fromInputStream(iStream)
      val lines = resource.getLines().toVector
      val nameProgram = lines(0).replaceAll(" ", "").split(NAME_KEY)(1)
      val dimension = lines(3).replaceAll(" ", "").split(DIMENSION_KEY)(1).trim
      val edgeWeightType = lines(4).replaceAll(" ", "").split(EDGE_WEIGHT_TYPE)(1).trim
      val nodePtr = setNodePtr(lines, dimension.toInt)
      resource.close()
      println("done ..")
      TspParameters(nameProgram, dimension.toInt, selectDistance(edgeWeightType), nodePtr, new Random(seed), 1, 1, 0.05, Vector.empty)
    } catch {
      case x: Exception =>
        println("\nError reading file " + filename + "...")
        throw new IllegalArgumentException
    }
  }

  def writeReport(sparkContext: Option[SparkContext], name: String, ep: ExecutionParameters): Unit = {
    var file: File = null
    if (sparkContext.isEmpty) {
      file = new File("best." + name + ".txt")
    } else {
      if (!ep.isColonies.equals(0)) {
        file = new File("best-colonies-" + sparkContext.get.defaultParallelism + "-colonies-" + ep.coloniesIterations + "-" + name + ".txt")
      } else {
        file = new File("best-master-slave-" + sparkContext.get.defaultParallelism + "-worker-" + name + ".txt")
      }
    }
    val outputFile = new BufferedWriter(new FileWriter(file))
    writeParametersSettings(outputFile, ep)
    for (i <- 0 until ep.maxTries) {
      outputFile.write("Try " + i + "\tBest " + _bestInTry(i).get + "\tIterations " + _bestFoundAt(i).get + "\tTime "
        + _timeBestFound(i).get + "\tTot.time " + _timeTotalRun(i).get + "\n")
    }
    outputFile.write("\n\n")
    val bestTourLength = _bestInTry.minBy(_.get)
    val worstTourLength = _bestInTry.maxBy(_.get)
    val medianBestValueTries = _bestInTry.sortWith(_.get < _.get).drop(_bestInTry.length / 2).head
    val medianBestTimeTries = _timeBestFound.sortWith(_.get < _.get).drop(_timeBestFound.length / 2).head
    val medianTimeTotalRun = _timeTotalRun.sortWith(_.get < _.get).drop(_timeTotalRun.length / 2).head
    outputFile.write("Best try " + bestTourLength + "\t")
    outputFile.write("Worst try " + worstTourLength + "\n")
    outputFile.write("Median values tries : " + medianBestValueTries + "\n")
    outputFile.write("Median best time tries : " + medianBestTimeTries + "\n")
    outputFile.write("Median total time tries : " + medianTimeTotalRun + "\n")
    outputFile.write("End problem : " + name)
    outputFile.close()
    println("Best try: " + bestTourLength + " Worst try: " + worstTourLength)
    println("End problem " + name)
  }

  def checkTour(tour: Vector[Option[Int]], numberCities: Int): Unit = {
    val vectSum: Vector[Int] = tour.map(e => e.getOrElse(0))
    val sum: Int = vectSum.dropRight(1).reduce((x, y) => x + y)
    if (sum != ((numberCities - 1) * numberCities) / 2) {
      println("Next tour must be flawed !!")
      println("Tour sum: " + sum)
    }
  }

  def writeParametersSettings(output: BufferedWriter, ep: ExecutionParameters): Unit = {
    output.write("Parameter-settings:\n\n")
    output.write("maxTries -> " + ep.maxTries + "\n")
    output.write("maxTours -> " + ep.maxTours + "\n")
    output.write("maxTime -> " + ep.maxTime + "\n")
    output.write("seed -> " + ep.seed + "\n")
    output.write("optimum -> " + ep.optimal + "\n")
    output.write("nAnts -> " + ep.nAnts + "\n")
    output.write("nnAnts -> " + ep.nnAnts + "\n")
    output.write("alpha -> " + ep.alpha + "\n")
    output.write("beta -> " + ep.beta + "\n")
    output.write("rho -> " + ep.rho + "\n")
    output.write("q0 -> " + ep.q0 + "\n")
    output.write("elitistAnts -> " + ep.elitistsAnts + "\n")
    output.write("rasRanks -> " + ep.rasRanks + "\n")
    output.write("lsFlag -> " + ep.lsFlag + "\n")
    output.write("nnLs -> " + ep.nnLs + "\n")
    output.write("asFlag -> " + ep.asFlag + "\n")
    output.write("mmasFlag -> " + ep.mmasFlag + "\n")
    output.write("coloniesIterations -> " + ep.coloniesIterations + "\n\n")
  }

  def printParameters(ep: ExecutionParameters): Unit = {
    println("\nExecution parameters:")
    println("maxTries -> " + ep.maxTries)
    println("maxTours -> " + ep.maxTours)
    println("maxTime -> " + ep.maxTime)
    println("seed -> " + ep.seed)
    println("optimum -> " + ep.optimal)
    println("nAnts -> " + ep.nAnts)
    println("nnAnts -> " + ep.nnAnts)
    println("alpha -> " + ep.alpha)
    println("beta -> " + ep.beta)
    println("rho -> " + ep.rho)
    println("q0 -> " + ep.q0)
    println("elitistAnts -> " + ep.elitistsAnts)
    println("rasRanks -> " + ep.rasRanks)
    println("lsFlag -> " + ep.lsFlag)
    println("nnLs -> " + ep.nnLs)
    println("asFlag -> " + ep.asFlag)
    println("mmasFlag -> " + ep.mmasFlag)
    println("coloniesIterations -> " + ep.coloniesIterations)
    println("\n")
  }

}
