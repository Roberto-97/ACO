package Main


import Entities.{Aco, Ant, ExecutionParameters, LocalSearch, Tsp}
import Util.Conf
import Util.Timer.{elapsedTime, startTimer}
import Util.InOut.initProgram

object Main {


  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    conf.build
    startTimer()
    initProgram()
    Tsp.computeNearestNeighborsMatrix()
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")
    (0 to  ExecutionParameters.maxTries).map((nTry) => {
      Tsp.initTry()
      while (!Aco.terminationCondition()){
        Aco.constructSolutions()
        if (ExecutionParameters.lsFlag != 0) {
          /* TODO: Local search */
        }
        Aco.updateStatistics()

      }
    })

  }


}
