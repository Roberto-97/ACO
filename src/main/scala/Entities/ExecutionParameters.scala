package Entities

case class ExecutionParameters(lsFlag: Int,
                               nnLs: Int,
                               nAnts: Int,
                               nnAnts: Int,
                               rho: Double,
                               alpha: Double,
                               beta: Double,
                               q0: Double,
                               maxTries: Int,
                               maxTours: Int,
                               maxTime: Double,
                               optimal: Int,
                               asFlag: Boolean,
                               dlbFlag: Boolean,
                               easFlag: Boolean,
                               rasFlag: Boolean ,
                               mmasFlag: Boolean,
                               bwasFlag: Boolean,
                               acsFlag: Boolean,
                               trailMax: Boolean,
                               trailMix: Boolean,
                               ugb: Int,
                               trail0: Double)
