package Alg

import Entities.Tsp.{computeDistances, nodeBranching}
import Entities._
import Util.InOut._
import Util.Timer.{elapsedTime, startTimer}
import org.apache.spark.SparkContext

import scala.util.Random

class AcoColonies extends Aco with Serializable {

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
    var colonies: Vector[Colonie] = Vector.fill(sparkContext.get.defaultParallelism)(new Colonie().initializeColonie(ep, tspParameters.numberCities))
    (0 until ep.maxTries).map(nTry => {
      println("Begin try " + nTry + " \n")
      initTry(ep, tspParameters, colonies)
      masterColonie.bestSoFarAnt.tourLength = Option(Int.MaxValue)
      while (!terminationCondition(masterColonie, ep, tspParameters)) {
        colonies = sparkContext.get.parallelize(colonies).mapPartitions(iterator => {
          iterator.map(colonie => {
            executeAcoColonies(masterColonie.bestSoFarAnt, colonie, nTry, ep, tspParameters)
          })
        }).collect().toVector
        val bestColonie = colonies.reduce((c1, c2) => if (c1.bestSoFarAnt.tourLength.get < c2.bestSoFarAnt.tourLength.get) c1 else c2)
        if (bestColonie.bestSoFarAnt.tourLength.get < masterColonie.bestSoFarAnt.tourLength.get) {
          bestColonie.bestSoFarAnt.clone(masterColonie.bestSoFarAnt)
          masterColonie.foundBest = tspParameters.iteration
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
    bestAnt.clone(colonie.bestSoFarAnt)
    for (k <- 0 to ep.coloniesIterations) {
      constructSolutions(colonie, ep, tspParameters, null)
      updateStatistics(colonie, ep, tspParameters)
      pheromoneTrailUpdate(colonie, ep, tspParameters)
    }
    searchControlAndStatistics(nTry, colonie, ep, tspParameters)
    colonie
  }

  override def searchControlAndStatistics(nTry: Int, colonie: Colonie, ep: ExecutionParameters, tspParameters: TspParameters): Unit = {
    colonie.branchingFactor = nodeBranching(colonie, ep, tspParameters)
    if (ep.mmasFlag != 0 && (colonie.branchingFactor < ep.branchFac) && (tspParameters.iteration - colonie.restartFoundBest > 2)) {
      println("INIT TRAILS !!!")
      colonie.restartBestAnt.tourLength = Option(Int.MaxValue)
      colonie.initPheromoneTrails(ep.trailMax, tspParameters.numberCities)
      colonie.computeTotalInformation(ep, tspParameters.numberCities, tspParameters.distance)
      colonie.restartIteration = tspParameters.iteration
    }
    println("try " + nTry + ", iteration " + tspParameters.iteration)
  }

}
