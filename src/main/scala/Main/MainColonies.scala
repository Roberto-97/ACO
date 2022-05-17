package Main

import Alg.AcoColonies
import Util.Conf
import org.apache.spark.{SparkConf, SparkContext}

object MainColonies {

  def main(args: Array[String]): Unit = {
    val ep = new Conf(args).build
    ep.isColonies = 1
    val sparkConf = new SparkConf().setAppName("aco-spark")
    val sc = new SparkContext(sparkConf)
    new AcoColonies().run(ep, Option(sc))
  }

}
