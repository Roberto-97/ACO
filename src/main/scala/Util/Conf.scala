package Util

import Entities.ExecutionParameters
import org.rogach.scallop.{ScallopConf, doubleConverter, intConverter, longConverter}

class Conf(args: Seq[String]) extends ScallopConf(args) with Serializable {
  val lsFlag = opt[Int](required = false, validate = _ > 0)
  val nnLs = opt[Int](required = false, validate = _ > 0)
  val numberAnts = opt[Int](required = false, validate = _ > 0)
  val numberNeighboursAnts = opt[Int](required = false, validate = _ > 0)
  val rho = opt[Double](required = false, validate = _ > 0)
  val alpha = opt[Double](required = false, validate = _ > 0)
  val beta = opt[Double](required = false, validate = _ > 0)
  val q0 = opt[Double](required = false, validate = _ > 0)
  val maxTries = opt[Int](required = false, validate = _ > 0)
  val maxTours = opt[Int](required = false, validate = _ > 0)
  val maxTime = opt[Double](required = false, validate = _ > 0)
  val optimal = opt[Int](required = false, validate = _ > 0)
  val asFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val mmasFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val trailMax = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val trailMix = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val ugb = opt[Int](required = false)
  val trail0 = opt[Double](required = false, validate = f => f >= 0 && f <= 1)
  val rasRanks = opt[Int](required = false, validate = _ > 0)
  val elitistAnts = opt[Int](required = false, validate = _ > 0)
  val tsplibfile = opt[String](required = true)
  val branchFac = opt[Double](required = false, validate = _ > 0)
  val seed = opt[Long](required = false, validate = _ > 0)
  val maxIterations = opt[Int](required = false, validate = _ > 0)
  val coloniesIterations = opt[Int](required = false, validate = _ > 0)
  val isColonies = opt[Int](required = false, validate = _ > 0)
  verify()

  def build: ExecutionParameters = {
    ExecutionParameters(lsFlag.getOrElse(0), nnLs.getOrElse(20),
      numberAnts.getOrElse(25), numberNeighboursAnts.getOrElse(20), rho.getOrElse(0.02),
      alpha.getOrElse(1.0), beta.getOrElse(2.0), q0.getOrElse(0.0), maxTries.getOrElse(10),
      maxTours.getOrElse(0), maxTime.getOrElse(10.0), optimal.getOrElse(1), asFlag.getOrElse(0),
      mmasFlag.getOrElse(1), ugb.getOrElse(Int.MaxValue), rasRanks.getOrElse(0), elitistAnts.getOrElse(0), tsplibfile.apply(),
      branchFac.getOrElse(1.00001), seed.getOrElse(System.nanoTime()), maxIterations.getOrElse(1000), coloniesIterations.getOrElse(10), isColonies.getOrElse(0))

  }


}
