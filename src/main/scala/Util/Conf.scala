package Util

import Entities.ExecutionParameters
import org.rogach.scallop.{ScallopConf, intConverter, doubleConverter, flagConverter, stringConverter}

class Conf (args: Seq[String]) extends ScallopConf(args) with Serializable{
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
  val dlbFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val easFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val rasFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val mmasFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val bwasFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val acsFlag = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val trailMax = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val trailMix = opt[Int](required = false, validate = f => f >= 0 && f <= 1)
  val ugb = opt[Int](required = false)
  val trail0 = opt[Double](required = false, validate = f => f >= 0 && f <= 1)
  val rasRanks = opt[Int](required = false, validate = _ > 0)
  val elitistAnts = opt[Int](required = false, validate = _ > 0)
  val tsplibfile = opt[String](required = true)
  verify()

  def setDefaultAsParameters(ep: ExecutionParameters): Unit = {
    if (ep.asFlag != 0) {
      ep.nAnts = numberAnts.getOrElse(-1)
      ep.nnAnts = numberNeighboursAnts.getOrElse(20)
      ep.alpha = alpha.getOrElse(1.0)
      ep.beta = beta.getOrElse(2.0)
      ep.rho = rho.getOrElse(0.5)
      ep.q0 = q0.getOrElse(0.0)
      ep.rasRanks = rasRanks.getOrElse(0)
      ep.elitistRanks = elitistAnts.getOrElse(0)
    }
  }

  def setDefaulEasParameters(ep: ExecutionParameters): Unit = {
    if (ep.easFlag != 0) {
      ep.nAnts = numberAnts.getOrElse(-1)
      ep.nnAnts = numberNeighboursAnts.getOrElse(20)
      ep.alpha = alpha.getOrElse(1.0)
      ep.beta = beta.getOrElse(2.0)
      ep.rho = rho.getOrElse(0.5)
      ep.q0 = q0.getOrElse(0.0)
      ep.rasRanks = rasRanks.getOrElse(0)
      ep.elitistRanks = elitistAnts.getOrElse(ep.nnAnts)
    }
  }

  def setDefaulRasParameters(ep: ExecutionParameters): Unit = {
    if (ep.rasFlag != 0) {
      ep.nAnts = numberAnts.getOrElse(-1)
      ep.nnAnts = numberNeighboursAnts.getOrElse(20)
      ep.alpha = alpha.getOrElse(1.0)
      ep.beta = beta.getOrElse(2.0)
      ep.rho = rho.getOrElse(0.1)
      ep.q0 = q0.getOrElse(0.0)
      ep.rasRanks = rasRanks.getOrElse(6)
      ep.elitistRanks = elitistAnts.getOrElse(0)
    }
  }

  def setDefaultMmasParameters(ep: ExecutionParameters): Unit = {
    if (ep.mmasFlag != 0) {
      ep.nAnts = numberAnts.getOrElse(-1)
      ep.nnAnts = numberNeighboursAnts.getOrElse(20)
      ep.alpha = alpha.getOrElse(1.0)
      ep.beta = beta.getOrElse(2.0)
      ep.rho = rho.getOrElse(0.02)
      ep.q0 = q0.getOrElse(0.0)
      ep.rasRanks = rasRanks.getOrElse(0)
      ep.elitistRanks = elitistAnts.getOrElse(0)
    }
  }

  def setDefaultAcsParameters(ep: ExecutionParameters): Unit = {
    if (ep.acsFlag != 0) {
      ep.nAnts = numberAnts.getOrElse(10)
      ep.nnAnts = numberNeighboursAnts.getOrElse(20)
      ep.alpha = alpha.getOrElse(1.0)
      ep.beta = beta.getOrElse(2.0)
      ep.rho = rho.getOrElse(0.1)
      ep.q0 = q0.getOrElse(0.9)
      ep.rasRanks = rasRanks.getOrElse(0)
      ep.elitistRanks = elitistAnts.getOrElse(0)
    }
  }

  def setDefaultLsParameters(ep: ExecutionParameters): Unit = {
    val lsFlagValues = Vector(0,1,2,3)
    if (lsFlagValues.contains(ep.lsFlag)) {
      ep.dlbFlag = 1
      ep.nnLs = nnLs.getOrElse(20)
      ep.nAnts = numberAnts.getOrElse(25)
      ep.nnAnts = numberNeighboursAnts.getOrElse(20)
      ep.alpha = alpha.getOrElse(1.0)
      ep.beta = beta.getOrElse(2.0)
      ep.rho = rho.getOrElse(0.5)
      ep.q0 = q0.getOrElse(0.0)
      if (ep.mmasFlag != 0) {
        ep.nAnts = numberAnts.getOrElse(25)
        ep.rho = rho.getOrElse(0.2)
        ep.q0 = q0.getOrElse(0.0)
      } else if (ep.acsFlag != 0) {
        ep.nAnts = numberAnts.getOrElse(10)
        ep.rho = rho.getOrElse(0.1)
        ep.q0 = q0.getOrElse(0.98)
      } else if (ep.easFlag != 0) {
        ep.elitistRanks = elitistAnts.getOrElse(ep.nAnts)
      }
    }
  }

  def setDefaultParameters(ep: ExecutionParameters): ExecutionParameters = {
    setDefaultAsParameters(ep)
    setDefaulEasParameters(ep)
    setDefaulRasParameters(ep)
    setDefaultMmasParameters(ep)
    setDefaultAcsParameters(ep)
    setDefaultLsParameters(ep)
    ep
  }

  def build: ExecutionParameters = {
    val ep = ExecutionParameters(lsFlag.getOrElse(3), nnLs.getOrElse(20),
      numberAnts.getOrElse(25), numberNeighboursAnts.getOrElse(20), rho.getOrElse(0.5),
      alpha.getOrElse(1.0), beta.getOrElse(2.0), q0.getOrElse(0.0), maxTries.getOrElse(10),
      maxTours.getOrElse(0), maxTime.getOrElse(10.0), optimal.getOrElse(1), asFlag.getOrElse(0), dlbFlag.getOrElse(1),
      easFlag.getOrElse(0), rasFlag.getOrElse(0), mmasFlag.getOrElse(1), bwasFlag.getOrElse(0), acsFlag.getOrElse(0),
      trailMax.getOrElse(0), trailMix.getOrElse(0), ugb.getOrElse(Int.MaxValue),
      trail0.getOrElse(0.0), rasRanks.getOrElse(0), elitistAnts.getOrElse(0), tsplibfile.apply())
      setDefaultParameters(ep)
  }


}
