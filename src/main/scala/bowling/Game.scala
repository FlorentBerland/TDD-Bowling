package bowling

import language.postfixOps
import scala.annotation.tailrec


/**
  * A bowling game model class
  *
  * @param frames The frames of the game
  */
case class Game private (frames: List[Frame]){

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

object Game {

  def apply(frames: List[Frame]): Option[Game] = if(frames.size > 10) None else Some(new Game(frames))

  /**
    * Generates a pseudo-random Game
    */
  def generate: Game = new Game(List.fill(9)(Frame.generate) :+ Frame.generateLast)


  /**
    * Generates a game with only strikes
    */
  def generateAllStrikes: Game = new Game(List.fill(9)(Frame.generateStrike) :+ Frame(List.fill(3)(Roll.generateStrike)))


  /**
    * Generates a game with only miss
    */
  def generateAllMiss: Game = new Game(List.fill(10)(Frame.generateMiss))
}