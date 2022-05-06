package Util

import Entities.ExecutionParameters
import org.rogach.scallop.{ScallopConf, doubleConverter, intConverter, longConverter, stringConverter}

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
  val workers = opt[Int](required = false, validate = _ > 0)
  verify()

  def setDefaultAsParameters(): Unit = {
    if (ExecutionParameters.asFlag != 0) {
      ExecutionParameters.nAnts = numberAnts.getOrElse(-1)
      ExecutionParameters.nnAnts = numberNeighboursAnts.getOrElse(20)
      ExecutionParameters.alpha = alpha.getOrElse(1.0)
      ExecutionParameters.beta = beta.getOrElse(2.0)
      ExecutionParameters.rho = rho.getOrElse(0.5)
      ExecutionParameters.q0 = q0.getOrElse(0.0)
      ExecutionParameters.rasRanks = rasRanks.getOrElse(0)
      ExecutionParameters.elitistsAnts = elitistAnts.getOrElse(0)
    }
  }

  def setDefaultMmasParameters(): Unit = {
    if (ExecutionParameters.mmasFlag != 0) {
      ExecutionParameters.nAnts = numberAnts.getOrElse(-1)
      ExecutionParameters.nnAnts = numberNeighboursAnts.getOrElse(20)
      ExecutionParameters.alpha = alpha.getOrElse(1.0)
      ExecutionParameters.beta = beta.getOrElse(2.0)
      ExecutionParameters.rho = rho.getOrElse(0.02)
      ExecutionParameters.q0 = q0.getOrElse(0.0)
      ExecutionParameters.rasRanks = rasRanks.getOrElse(0)
      ExecutionParameters.elitistsAnts = elitistAnts.getOrElse(0)
    }
  }

  def setDefaultLsParameters(): Unit = {
    if (ExecutionParameters.lsFlagValues.contains(ExecutionParameters.lsFlag)) {
      ExecutionParameters.nnLs = nnLs.getOrElse(20)
      ExecutionParameters.nAnts = numberAnts.getOrElse(25)
      ExecutionParameters.nnAnts = numberNeighboursAnts.getOrElse(20)
      ExecutionParameters.alpha = alpha.getOrElse(1.0)
      ExecutionParameters.beta = beta.getOrElse(2.0)
      ExecutionParameters.rho = rho.getOrElse(0.5)
      ExecutionParameters.q0 = q0.getOrElse(0.0)
      if (ExecutionParameters.mmasFlag != 0) {
        ExecutionParameters.nAnts = numberAnts.getOrElse(25)
        ExecutionParameters.rho = rho.getOrElse(0.2)
        ExecutionParameters.q0 = q0.getOrElse(0.0)
      }
    }
  }

  def setDefaultParameters(): Unit = {
    setDefaultAsParameters()
    setDefaultMmasParameters()
    setDefaultLsParameters()
  }

  def build: Unit = {
    ExecutionParameters.init(lsFlag.getOrElse(0), nnLs.getOrElse(20),
      numberAnts.getOrElse(25), numberNeighboursAnts.getOrElse(20), rho.getOrElse(0.5),
      alpha.getOrElse(1.0), beta.getOrElse(2.0), q0.getOrElse(0.0), maxTries.getOrElse(10),
      maxTours.getOrElse(0), maxTime.getOrElse(10.0), optimal.getOrElse(1), asFlag.getOrElse(0),
      mmasFlag.getOrElse(1), trailMax.getOrElse(0), trailMix.getOrElse(0), ugb.getOrElse(Int.MaxValue),
      trail0.getOrElse(0.0), rasRanks.getOrElse(0), elitistAnts.getOrElse(0), tsplibfile.apply(),
      branchFac.getOrElse(1.00001), seed.getOrElse(System.nanoTime()), maxIterations.getOrElse(1000), coloniesIterations.getOrElse(10), workers.getOrElse(1))
    setDefaultParameters()
  }


}
