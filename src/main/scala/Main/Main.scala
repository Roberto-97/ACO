package Main


import Entities.{Aco, Ant, LocalSearch, Tsp}
import Util.Conf
import Util.Timer.{elapsedTime, startTimer}
import Util.Parser.initProgram

object Main {
  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    startTimer()
    initProgram(conf.build)
    val time_used = elapsedTime()
    println("\nInitialization took " + time_used + " seconds\n")

  }


}
