package util

import scala.util.Random

/**
  * Generates random number, should be used instead of recreating a scala.util.Random object
  */
object Rand {

  private val _rand = new Random()

  /**
    * Generate a pseudo-random number between 0 (included) and n (excluded)
    *
    * @param n The range of the random number
    * @return A pseudo-random number
    */
  def nextInt(n: Int): Int = _rand.nextInt(n)

}
