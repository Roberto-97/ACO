package Main


import Entities.Aco.AcoOperations._
import Entities.Colonie
import Entities.ExecutionParameters._
import Entities.Tsp.computeNearestNeighborsMatrix
import Util.Conf
import Util.InOut._
import Util.Timer.{elapsedTime, startTimer}

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    conf.build
    startTimer()
    initProgram()
    computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    val colonie: Colonie = new Colonie().initializeColonie()
    println("\nInitialization took " + time_used + " seconds\n")
    (0 until maxTries).map(nTry => {
      initTry(nTry, Vector(colonie))
      while (!terminationCondition(colonie)) {
        constructSolutions(colonie)
        updateStatistics(colonie)
        pheromoneTrailUpdate(colonie)
        searchControlAndStatistics(nTry, colonie)
        iteration += 1
      }
      exitTry(nTry, colonie)
    })
    exitProgram(false)
  }


}
