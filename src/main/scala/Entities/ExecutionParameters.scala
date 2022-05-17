package Entities

case class ExecutionParameters(lsFlag: Int, var nnLs: Int, var nAnts: Int, var nnAnts: Int, rho: Double, alpha: Double, beta: Double, q0: Double,
                               maxTries: Int, maxTours: Int, maxTime: Double, optimal: Int, asFlag: Int, mmasFlag: Int, var trailMax: Double,
                               var trailMin: Double, var ugb: Int, var trail0: Double, rasRanks: Int, elitistsAnts: Int, tsplibfile: String,
                               branchFac: Double, var seed: Long, maxIterations: Int, coloniesIterations: Int, var isColonies: Int)