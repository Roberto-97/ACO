package Alg

import Alg.Aco._
import Entities.Tsp.computeDistances
import Entities._
import Util.InOut._
import Util.Timer.{elapsedTime, startTimer}
import org.apache.spark.SparkContext


class AcoColonies {

  def run(ep: ExecutionParameters, sparkContext: Option[SparkContext]): Unit = {
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


}
