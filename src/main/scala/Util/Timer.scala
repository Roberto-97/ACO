package Util

object Timer {

  private var timer: Double = Double.NaN

  private val CLOCKS_PER_SEC: Double = 1000000000


  def startTimer(): Unit = {
    timer = System.nanoTime()
  }

  def elapsedTime(): Double = {
    timer = System.nanoTime() - timer
    timer / CLOCKS_PER_SEC
  }

}
