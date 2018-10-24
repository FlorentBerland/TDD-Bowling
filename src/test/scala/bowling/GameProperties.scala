package bowling

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object GameProperties extends Properties("Game") {

  property("apply") = forAll(Game(List.fill(11)(Frame.generate))) { g =>
    g.isEmpty
  }

  property("score") = forAll(Game.generateAllStrikes) { g =>
    g.score == 300
  }

  property("score") = forAll(Game(List.fill(10)(Frame.generateRegular))) { game =>
    game match {
      case Some(g) => g.score == scoreOf(g.frames)
      case None => false
    }
  }


  /**
    * Compute the score of a list of frames, assuming they are all regular
    */
  def scoreOf(frames: List[Frame]): Int = frames.foldLeft(0)((acc, frame) => acc + frame.score(Nil)._1)

}
