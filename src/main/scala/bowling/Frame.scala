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

object Frame {

  /**
    * Generates a pseudo-random Frame
    */
  def generate: Frame = {
    val firstRoll = Roll.generate
    if(firstRoll.pinsHit < 10)
      Frame(firstRoll :: Nil)
    else
      Frame(firstRoll :: Roll.generate(firstRoll.pinsHit) :: Nil)
  }


  /**
    * Generates a pseudo-random last Frame (containing one more opportunity in case of a spare
    * and 2 for a strike)
    */
  def generateLast: Frame = {
    val firstFrame = generate
    if(firstFrame.isStrike){
      val secondFrame = generate
      if(secondFrame.isStrike)
        Frame(firstFrame.rolls ++ secondFrame.rolls :+ Roll.generate)
      else
        Frame(firstFrame.rolls ++ secondFrame.rolls)
    } else {
      if(firstFrame.isSpare)
        Frame(firstFrame.rolls :+ Roll.generate)
      else
        firstFrame
    }
  }


  /**
    * Generates a strike Frame
    */
  def generateStrike: Frame = Frame(List(Roll.generateStrike))


  /**
    * Generates a pseudo-random spare Frame
    */
  def generateSpare: Frame = Frame(Roll.generateSpare)


  /**
    * Generates an all miss frame
    */
  def generateMiss: Frame = Frame(List(Roll.generateMiss, Roll.generateMiss))


  /**
    * Generates a regular frame, can be missed
    */
  def generateRegular: Frame = Frame(Roll.generateRegular)
}