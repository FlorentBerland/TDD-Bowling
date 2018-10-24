package bowling

/**
  * A frame of the game
  *
  * @param rolls The rolls performed in the frame
  */
case class Frame(rolls: List[Roll]) {

  /**
    * Computes the score of the frame considering optional bonuses
    *
    * @param bonusDurations A list of bonus durations, created by previous spares or strikes
    *                       Each roll depends on the active bonuses
    * @return The score of the frame and the bonuses durations updated after the frame
    */
  def score(bonusDurations: List[Int]): (Int, List[Int]) = {
    rolls.foldLeft((0, bonusDurations))((acc, roll) => {
      (acc._1 + roll.pinsHit * (1 + acc._2.count(_ > 0)), acc._2.map(_ - 1))
    })
  }


  /**
    * Tests whether the frame is a strike or not
    */
  def isStrike: Boolean = rolls.headOption.exists(_.pinsHit == 10)


  /**
    * Tests whether the frame is a spare or not
    */
  def isSpare: Boolean = rolls match {
    case firstRoll :: secondRoll :: _ => firstRoll.pinsHit + secondRoll.pinsHit == 10
    case _ => false
  }

}