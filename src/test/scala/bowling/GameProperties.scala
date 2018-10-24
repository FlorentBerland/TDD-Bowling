package bowling

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object GameProperties extends Properties("Game") {

  property("score") = forAll(Game.generateAllStrikes) { g =>
    g.score == 300
  }

  property("score") = forAll(Game(List.fill(10)(Frame.generateRegular))) { g =>
    g.score == scoreOf(g.frames)
  }


  /**
    * Compute the score of a list of frames, assuming they are all regular
    */
  def scoreOf(frames: List[Frame]): Int = frames.foldLeft(0)((acc, frame) => acc + frame.score(Nil)._1)

}
