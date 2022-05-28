package Main

import Util.Conf
import org.apache.spark.{SparkConf, SparkContext}

object MainMasterSlave {

  def main(args: Array[String]): Unit = {
    val ep = new Conf(args).build
    val sparkConf = new SparkConf().setAppName("aco-spark")
    val sc = new SparkContext(sparkConf)
    //    new AcoMasterSlave().run(ep, Option(sc))
  }

}
