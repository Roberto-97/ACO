package Main

import Alg.AcoSec
import Util.Conf

object Main {

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    new AcoSec().run(conf.build, Option.empty)
  }

}
