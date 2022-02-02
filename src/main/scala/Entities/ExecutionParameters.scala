package Entities

case class ExecutionParameters(nAnts: Int = 25,
                               nnAnts: Int = 20,
                               rho: Double,
                               alpha: Double = 1,
                               beta: Double = 2,
                               q0: Double = 0.0,
                               asFlag: Boolean = true,
                               easFlag: Boolean = false,
                               rasFlag: Boolean = false,
                               mmasFlag: Boolean = false,
                               bwasFlag: Boolean = false,
                               acsFlag: Boolean = false,
                               trailMax: Boolean = false,
                               trailMix: Boolean = false,
                               ugb: Int = 0,
                               trail0: Double)
