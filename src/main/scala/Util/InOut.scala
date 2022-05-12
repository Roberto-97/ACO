package Util

import Entities.Aco.AcoOperations
import Entities.Colonie
import Entities.DistanceStrategies._
import Entities.ExecutionParameters._
import Entities.Tsp._
import Util.SparkConf.getSparkContext
import Util.Timer.{elapsedTime, startTimer}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

object InOut {

  private val NAME_KEY = "NAME:"
  private val DIMENSION_KEY = "DIMENSION:"
  private val EDGE_WEIGHT_TYPE = "EDGE_WEIGHT_TYPE:"

  private var _nTry: Int = 0
  protected var _nTours: Int = 0
  protected var _iteration: Int = 0
  protected var _restartIteration: Int = 0
  private var _restartTime: Double = 0.0
  protected var _lambda: Double = 0.0
  protected var _foundBest: Int = 0
  private var _restartFoundBest: Int = 0
  protected var _bestInTry: Vector[Option[Int]] = Vector.empty
  protected var _bestFoundAt: Vector[Option[Int]] = Vector.empty
  protected var _timeBestFound: Vector[Option[Double]] = Vector.empty
  protected var _timeTotalRun: Vector[Option[Double]] = Vector.empty
  protected var _timeUsed: Double = Double.NaN
  private var _foundBranching: Double = Double.NaN
  private var _branchingFactor: Double = Double.NaN
  protected var _timePassed: Double = Double.NaN

  /** *********************************************** Setters && Getters ****************************************************** */

  def nTry = _nTry

  def nTry_=(nTry: Int) = {
    _nTry = nTry
  }

  def nTours = _nTours

  def nTours_=(nTours: Int) = {
    _nTours = nTours
  }

  def iteration = _iteration

  def iteration_=(iteration: Int) = {
    _iteration = iteration
  }

  def restartIteration = _restartIteration

  def restartIteration_=(restartIteration: Int) = {
    _restartIteration = restartIteration
  }

  def restartTime = _restartTime

  def restartTime_=(restartTime: Double) = {
    _restartTime = restartTime
  }

  def lambda = _lambda

  def lambda_=(lambda: Double) = {
    _lambda = lambda
  }

  def foundBest = _foundBest

  def foundBest_=(foundBest: Int) = {
    _foundBest = foundBest
  }

  def restartFoundBest = _restartFoundBest

  def restartFoundBest_=(restartFoundBest: Int) = {
    _restartFoundBest = restartFoundBest
  }

  def bestInTry = _bestInTry

  def bestInTry_=(bestInTry: Vector[Option[Int]]) = {
    _bestInTry = bestInTry
  }

  def bestFoundAt = _bestFoundAt

  def bestFoundAt_=(bestFoundAt: Vector[Option[Int]]) = {
    _bestFoundAt = bestFoundAt
  }

  def timeBestFound = _timeBestFound

  def timeBestFound_=(timeBestFound: Vector[Option[Double]]) = {
    _timeBestFound = timeBestFound
  }

  def timeTotalRun = _timeTotalRun

  def timeTotalRun_=(timeTotalRun: Vector[Option[Double]]) = {
    _timeTotalRun = timeTotalRun
  }

  def timeUsed = _timeUsed

  def timeUsed_=(timeUsed: Double) = {
    _timeUsed = timeUsed
  }

  def foundBranching = _foundBranching

  def foundBranching_=(foundBranching: Double) = {
    _foundBranching = foundBranching
  }

  def branchingFactor = _branchingFactor

  def branchingFactor_=(branchingFactor: Double) = {
    _branchingFactor = branchingFactor
  }

  def timePassed = _timePassed

  def timePassed_=(timePassed: Double) = {
    _timePassed = timePassed
  }

  /** ************************************************************************************************************************* */

  def nodeBranching(lambda: Double, colonie: Colonie): Double = {
    var numBranches = Vector.fill(numberCities.get)(0.0)
    (0 until numberCities.get).map((m) => {
      var min = colonie.pheremone(m)(nearestNeighborsMatrix(m)(1).get)
      var max = colonie.pheremone(m)(nearestNeighborsMatrix(m)(1).get)
      (1 until nnAnts).map(i => {
        if (colonie.pheremone(m)(nearestNeighborsMatrix(m)(i).get) > max) {
          max = colonie.pheremone(m)(nearestNeighborsMatrix(m)(i).get)
        }
        if (colonie.pheremone(m)(nearestNeighborsMatrix(m)(i).get) < min) {
          min = colonie.pheremone(m)(nearestNeighborsMatrix(m)(i).get)
        }
      })
      val cutoff = min + lambda * (max - min)
      (0 until nnAnts).map(i => {
        if (colonie.pheremone(m)(nearestNeighborsMatrix(m)(i).get) > cutoff)
          numBranches = numBranches.updated(m, numBranches(m) + 1.0)
      })
    })
    val avg = numBranches.reduce((x, y) => x + y)
    avg / (numberCities.get * 2)
  }

  def initProgram(): Unit = {
    initializeParams()
    readEtsp(tsplibfile)
    if (nAnts < 0)
      nAnts = numberCities.get

    nnLs = (numberCities.get - 1).min(nnLs)

    println("Calculating distance matrix ..")
    computeDistances()
    println("done ..")
    printParameters()
  }

  def exitTry(nTry: Int, colonie: Colonie): Unit = {
    checkTour(colonie.bestSoFarAnt.tour)
    println("Best Solution in try " + nTry + " is " + colonie.bestSoFarAnt.tourLength)
    println("Best Solution was found after " + _foundBest + " iterations")
    _bestInTry = _bestInTry.updated(nTry, Option(colonie.bestSoFarAnt.tourLength.get))
    _bestFoundAt = _bestFoundAt.updated(nTry, Option(_foundBest))
    _timeBestFound = _timeBestFound.updated(nTry, Option(_timeUsed))
    _timeTotalRun = _timeTotalRun.updated(nTry, Option(elapsedTime()))
    println("Try " + nTry + ", Best " + _bestInTry(nTry) + ", found at iteration " + _bestFoundAt(nTry) + ", found at time " + _timeBestFound(nTry))
    println("End try")
  }

  def initTry(nTry: Int, colonies: Vector[Colonie]): Unit = {
    println("INITIALIZE TRIAL")
    startTimer()
    _timeUsed = elapsedTime()
    _timePassed = _timeUsed

    /* Initialize variables concerning statistics */
    _nTours = 1
    _iteration = 1
    _restartIteration = 1
    _lambda = 0.05
    colonies.map(colonie => colonie.bestSoFarAnt.tourLength = Option(Int.MaxValue))
    _foundBest = 0

    if (mmasFlag == 0) {
      colonies.map(colonie => {
        trail0 = 1.0 / (rho * AcoOperations.nnTour(colonie))
        colonie.initPheromoneTrails(trail0)
      })
    } else {
      colonies.map(colonie => {
        trailMax = 1.0 / (rho * AcoOperations.nnTour(colonie))
        trailMin = trailMax / (2 * numberCities.get)
        colonie.initPheromoneTrails(trailMax)
      })
    }

    /* Calculate combined information pheromone times heuristic information*/
    colonies.map(colonie => colonie.computeTotalInformation())

    println("Begin try " + nTry + " \n")
  }

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
      val iStream = getClass.getResourceAsStream("/" + filename)
      val resource = Source.fromInputStream(iStream)
      val lines = resource.getLines().toVector
      val name = lines(0).replaceAll(" ", "").split(NAME_KEY)(1)
      val dimension = lines(3).replaceAll(" ", "").split(DIMENSION_KEY)(1).trim
      val edgeWeightType = lines(4).replaceAll(" ", "").split(EDGE_WEIGHT_TYPE)(1).trim
      initializeTspParams(name, dimension.toInt, selectDistance(edgeWeightType))
      setNodePtr(lines)
      resource.close()
      println("done ..")
    } catch {
      case x: Exception =>
        println("\nError reading file " + filename + "...")
        throw new IllegalArgumentException
    }
  }

  def exitProgram(isColonies: Boolean): Unit = {
    writeReport(isColonies)
  }

  def writeReport(isColonies: Boolean): Unit = {
    var file: File = null
    if (Option(getSparkContext()).isEmpty) {
      file = new File("best." + name + ".txt")
    } else {
      if (isColonies) {
        file = new File("best-colonies-" + getSparkContext().defaultParallelism + "-worker-" + name + ".txt")
      } else {
        file = new File("best-master-slave-" + getSparkContext().defaultParallelism + "-worker-" + name + ".txt")
      }
    }
    val outputFile = new BufferedWriter(new FileWriter(file))
    writeParametersSettings(outputFile)
    for (i <- 0 until maxTries) {
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

  def checkTour(tour: Vector[Option[Int]]): Unit = {
    val vectSum: Vector[Int] = tour.map(e => e.getOrElse(0))
    val sum: Int = vectSum.dropRight(1).reduce((x, y) => x + y)
    if (sum != ((numberCities.get - 1) * numberCities.get) / 2) {
      println("Next tour must be flawed !!")
      printTour(tour)
      println("Tour sum: " + sum)
    }
  }

  def printTour(tour: Vector[Option[Int]]): Unit = {
    tour.map(city => println("" + city))
    println("Tour Length = " + computeTourLength(tour))
  }

  def writeParametersSettings(output: BufferedWriter): Unit = {
    output.write("Parameter-settings:\n\n")
    output.write("maxTries -> " + maxTries + "\n")
    output.write("maxTours -> " + maxTours + "\n")
    output.write("maxTime -> " + maxTime + "\n")
    output.write("seed -> " + seed + "\n")
    output.write("optimum -> " + optimal + "\n")
    output.write("nAnts -> " + nAnts + "\n")
    output.write("nnAnts -> " + nnAnts + "\n")
    output.write("alpha -> " + alpha + "\n")
    output.write("beta -> " + beta + "\n")
    output.write("rho -> " + rho + "\n")
    output.write("q0 -> " + q0 + "\n")
    output.write("elitistAnts -> " + elitistsAnts + "\n")
    output.write("rasRanks -> " + rasRanks + "\n")
    output.write("lsFlag -> " + lsFlag + "\n")
    output.write("nnLs -> " + nnLs + "\n")
    output.write("asFlag -> " + asFlag + "\n")
    output.write("mmasFlag -> " + mmasFlag + "\n\n")
  }

  def printParameters(): Unit = {
    println("\nExecution parameters:")
    println("maxTries -> " + maxTries)
    println("maxTours -> " + maxTours)
    println("maxTime -> " + maxTime)
    println("seed -> " + seed)
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

}
