package Main

import Alg.Aco
import Util.Conf

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    new Aco().run(conf.build, Option.empty)
  }

}
