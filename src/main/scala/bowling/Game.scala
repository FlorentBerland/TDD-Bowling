package bowling

import scala.annotation.tailrec


/**
  * A bowling game model class
  *
  * @param frames The frames of the game
  */
case class Game(frames: List[Frame]){

  /**
    * Computes the score of the game
    */
  def score: Int = {

    @tailrec
    def scoreRec(frames: List[Frame], score: Int, bonusDuration: List[Int]): Int = frames match {
      case Nil => score
      case head :: tail => // A bonus is applied to the roll(s) of the NEXT frame (to handle the last frame case)
        val (frameScore, consumedBonuses) = head.score(bonusDuration)
        scoreRec(tail, score + frameScore,
          if(head isStrike) 2 +: consumedBonuses
          else if(head isSpare) 1 +: consumedBonuses
          else consumedBonuses
        )
    }
    scoreRec(frames, 0, List.empty)
  }

}