package Util

import org.apache.spark.{SparkConf, SparkContext}

object SparkConf {
  private var sparkConf: SparkConf = null
  private var sparkContext: SparkContext = null


  def initializeSparkContext(): Unit = {
    this.sparkConf = new SparkConf().setAppName("aco-spark").setMaster("local[1]")
    this.sparkContext = new SparkContext(this.sparkConf)
  }

  def getSparkContext(): SparkContext = {
    this.sparkContext
  }

}
