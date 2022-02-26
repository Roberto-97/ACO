package Entities

object ExecutionParameters {
  var lsFlag: Int = 0
  var nnLs: Int = 0
  var nAnts: Int = 0
  var nnAnts: Int = 0
  var rho: Double = 0
  var alpha: Double = 0
  var beta: Double = 0
  var q0: Double = 0
  var maxTries: Int = 0
  var maxTours: Int = 0
  var maxTime: Double = 0
  var optimal: Int = 0
  var asFlag: Int = 0
  var mmasFlag: Int = 0
  var trailMax: Double = 0
  var trailMin: Double = 0
  var ugb: Int = 0
  var trail0: Double = 0
  var rasRanks: Int = 0
  var elitistsAnts: Int = 0
  var tsplibfile: String  = null
  var branchFac: Double = 0.0
  val lsFlagValues = Vector(0,1,2,3)

  def init(lsFlag: Int, nnLs: Int, nAnts: Int, nnAnts: Int, rho: Double, alpha: Double, beta: Double, q0: Double,
           maxTries: Int, maxTours: Int, maxTime: Double, optimal: Int, asFlag: Int, mmasFlag: Int, trailMax: Int,
           trailMin: Int, ugb: Int, trail0: Double, rasRanks: Int, elitistsAnts: Int, tsplibfile: String, branchFac: Double): Unit = {
    this.lsFlag = lsFlag
    this.nnLs = nnLs
    this.nAnts = nAnts
    this.nnAnts = nnAnts
    this.rho = rho
    this.alpha = alpha
    this.beta = beta
    this.q0 = q0
    this.maxTries = maxTries
    this.maxTours = maxTours
    this.maxTime = maxTime
    this.optimal = optimal
    this.asFlag = asFlag
    this.mmasFlag = mmasFlag
    this.trailMax = trailMax
    this.trailMin = trailMin
    this.ugb = ugb
    this.trail0 = trail0
    this.rasRanks = rasRanks
    this.elitistsAnts = elitistsAnts
    this.tsplibfile = tsplibfile
    this.branchFac = branchFac
  }


}
