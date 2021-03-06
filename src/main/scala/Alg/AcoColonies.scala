package Alg

import Entities.Tsp.computeDistances
import Entities._
import Util.InOut._
import Util.Timer.{elapsedTime, startTimer}
import org.apache.spark.SparkContext

import scala.util.Random


class AcoColonies extends Aco with Serializable {

  override def initTry(ep: ExecutionParameters, tspParameters: TspParameters, colonies: Vector[Colonie]): Unit = {
    println("INITIALIZE TRIAL")
    startTimer()

    /* Initialize variables concerning statistics */
    tspParameters.nTours = 1
    tspParameters.iteration = 1
    tspParameters.lambda = 0.05

  }

  override def run(ep: ExecutionParameters, sparkContext: Option[SparkContext]): Unit = {
    println("Number colonies -> " + sparkContext.get.defaultParallelism)
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
    val masterColonie: Colonie = new Colonie().initializeColonie(ep, tspParameters.numberCities)
    val colonies: Vector[Colonie] = Vector.fill(sparkContext.get.defaultParallelism)(new Colonie().initializeColonie(ep, tspParameters.numberCities))
    (0 until ep.maxTries).map(nTry => {
      println("Begin try " + nTry + " \n")
      initTry(ep, tspParameters, colonies)
      masterColonie.bestSoFarAnt.tourLength = Option(Int.MaxValue)
      val rddColonies = sparkContext.get.parallelize(colonies).cache()
      while (!terminationCondition(masterColonie, ep, tspParameters)) {
        val bestColonie = rddColonies.mapPartitions(iterator => {
          iterator.map(colonie => {
            executeAcoColonies(masterColonie.bestSoFarAnt, colonie, nTry, ep, tspParameters)
          })
        }).cache().reduce((c1, c2) => if (c1.bestSoFarAnt.tourLength.get < c2.bestSoFarAnt.tourLength.get) c1 else c2)
        if (bestColonie.bestSoFarAnt.tourLength.get < masterColonie.bestSoFarAnt.tourLength.get) {
          bestColonie.bestSoFarAnt.clone(masterColonie.bestSoFarAnt)
          masterColonie.foundBest = bestColonie.foundBest
          masterColonie.timeUsed = elapsedTime()
        }
        println("Best so far " + masterColonie.bestSoFarAnt.tourLength + ", iteration: " + tspParameters.iteration +
          ", time " + elapsedTime() + ", NTry " + nTry)
        tspParameters.iteration += 1
      }
      exitTry(nTry, masterColonie, tspParameters)
    })
    writeReport(sparkContext, tspParameters.name, ep)
  }

  def executeAcoColonies(bestAnt: Ant, colonie: Colonie, nTry: Int, ep: ExecutionParameters, tspParameters: TspParameters): Colonie = {
    tspParameters.randomNumber = new Random(System.nanoTime())
    if (bestAnt.tourLength.get == Int.MaxValue) {
      initPheromone(Vector(colonie), ep, tspParameters)
    }
    if (bestAnt.tourLength.get < colonie.bestSoFarAnt.tourLength.get) {
      bestAnt.clone(colonie.bestSoFarAnt)
      bestAnt.clone(colonie.restartBestAnt)
      colonie.trailMax = 1.0 / (ep.rho * colonie.bestSoFarAnt.tourLength.get)
      colonie.trailMin = colonie.trailMax / (2 * tspParameters.numberCities)
      colonie.trail0 = colonie.trailMax
    }
    val init = ((tspParameters.iteration - 1) * ep.coloniesIterations) + 1
    val fin = (tspParameters.iteration * ep.coloniesIterations)
    for (k <- init to fin) {
      tspParameters.iteration = k
      constructSolutions(colonie, ep, tspParameters, null)
      updateStatistics(colonie, ep, tspParameters)
      pheromoneTrailUpdate(colonie, ep, tspParameters)
      searchControlAndStatistics(nTry, colonie, ep, tspParameters)
    }
    colonie
  }


}
