package bowling

import util.Rand

/**
  * Represent a roll in the game
  *
  * @param pinsHit The number of pins hit in this roll
  */
case class Roll(pinsHit: Int)

object Roll {

  /**
    * Generate a pseudo-random roll
    */
  def generate: Roll = Roll(Rand.nextInt(11))

  /**
    * Generate a pseudo-random roll taking account on a previous roll
    *
    * @param n The pins hit by the previous roll
    * @return A roll on the remaining pins
    */
  def generate(n: Int): Roll = Roll(Rand.nextInt(11 - n))


  /**
    * Generate a roll that should be a strike
    */
  def generateStrike: Roll = Roll(10)


  /**
    * Generate two rolls that should be a spare
    */
  def generateSpare: List[Roll] = {
    val first = Roll(Rand.nextInt(10))
    val second = Roll(10 - first.pinsHit)
    first :: second :: Nil
  }


  /**
    * Generates a miss roll
    */
  def generateMiss: Roll = Roll(0)


  /**
    * Generates two regular rolls (can be missed)
    */
  def generateRegular: List[Roll] = {
    val first = Roll(Rand.nextInt(9))
    val second = Roll(Rand.nextInt(10 - first.pinsHit))
    first :: second :: Nil
  }

}