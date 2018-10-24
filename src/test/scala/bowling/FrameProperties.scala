package bowling

import language.postfixOps
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object FrameProperties extends Properties("Frame") {

  property("score") = forAll(Frame.generate) { f =>
    f.score(Nil)._1 == scoreOf(f.rolls)
  }

  property("score") = forAll(Frame.generateLast) { f =>
    f.score(Nil)._1 == scoreOf(f.rolls)
  }

  property("isSpare") = forAll(Frame.generate) { f =>
    f.rolls match {
      case first :: second :: _ => (scoreOf(first, second) == 10) == f.isSpare
      case _ => !f.isSpare
    }
  }

  property("isSpare") = forAll(Frame.generateSpare) { f =>
    f.rolls match {
      case first :: second :: _ => (scoreOf(first, second) == 10) && f.isSpare
      case _ => false
    }
  }

  property("isSpare") = forAll(Frame.generateLast) { f =>
    f.rolls match {
      case first :: second :: _ => (scoreOf(first, second) == 10) == f.isSpare
      case _ => !f.isSpare
    }
  }

  property("isStrike") = forAll(Frame.generate) { f =>
    f.rolls match {
      case first :: _ => (scoreOf(first) == 10) == f.isStrike
      case _ => !f.isStrike
    }
  }

  property("isStrike") = forAll(Frame.generateLast) { f =>
    f.rolls match {
      case first :: _ => (scoreOf(first) == 10) == f.isStrike
      case _ => !f.isStrike
    }
  }



  def scoreOf(rolls: List[Roll]): Int = rolls.foldLeft(0)((acc, value) => acc + value.pinsHit)
  def scoreOf(rolls: Roll*): Int = scoreOf(rolls.toList)

}
