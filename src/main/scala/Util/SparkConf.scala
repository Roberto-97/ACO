package Util

import org.apache.spark.{SparkConf, SparkContext}

object SparkConf {
  private var sparkConf: SparkConf = null
  private var sparkContext: SparkContext = null


  def initializeSparkContext(): Unit =  {
    this.sparkConf = new SparkConf().setAppName("aco-spark").setMaster("local[8]")
    this.sparkContext = new SparkContext(this.sparkConf)
    println("Num partitions : ", this.sparkContext.defaultParallelism)
  }

  def getSparkContext(): SparkContext = {
    this.sparkContext
  }

}
