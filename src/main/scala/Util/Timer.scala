package Util

object Timer {

  private var _startTime: Double = Double.NaN

  private var _elapsed: Double = Double.NaN

  private val CLOCKS_PER_SEC: Double = 1000000000


  def startTimer(): Unit = {
    _startTime = System.nanoTime()
  }

  def elapsedTime(): Double = {
    _elapsed = System.nanoTime() - _startTime
    _elapsed / CLOCKS_PER_SEC
  }

}
