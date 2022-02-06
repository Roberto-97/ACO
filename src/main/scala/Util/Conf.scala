package Util

import Entities.ExecutionParameters
import org.rogach.scallop.{ScallopConf, intConverter, doubleConverter, flagConverter}

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
  val asFlag = opt[Boolean](required = false)
  val dlbFlag = opt[Boolean](required = false)
  val easFlag = opt[Boolean](required = false)
  val rasFlag = opt[Boolean](required = false)
  val mmasFlag = opt[Boolean](required = false)
  val bwasFlag = opt[Boolean](required = false)
  val acsFlag = opt[Boolean](required = false)
  val trailMax = opt[Boolean](required = false)
  val trailMix = opt[Boolean](required = false)
  val ugb = opt[Int](required = false)
  val trail0 = opt[Double](required = false, validate = f => f >= 0 && f <= 1)
  verify()

  def build: ExecutionParameters = {
    ExecutionParameters(lsFlag.getOrElse(3), nnLs.getOrElse(20),
      numberAnts.getOrElse(25), numberNeighboursAnts.getOrElse(20), rho.getOrElse(0.5),
      alpha.getOrElse(1.0), beta.getOrElse(2.0), q0.getOrElse(0.0), maxTries.getOrElse(10),
      maxTours.getOrElse(0), maxTime.getOrElse(10.0), optimal.getOrElse(1), asFlag(), dlbFlag(),
      easFlag(), rasFlag(), mmasFlag(), bwasFlag(), acsFlag(), trailMax(), trailMix(), ugb.getOrElse(Int.MaxValue), trail0.getOrElse(0.0))
  }


}
