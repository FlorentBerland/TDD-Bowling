package bowling

import org.scalatest.WordSpec

class FrameSpec extends WordSpec {

  "A frame" when {
    "empty" should {
      "have score 0" in {
        assert(Frame(Nil).score(Nil)._1 == 0)
      }
      "not be a strike nor a spare" in {
        assert(!Frame(Nil).isStrike)
        assert(!Frame(Nil).isSpare)
      }
    }

    "has one roll of 10" should {
      "have score 10" in {
        assert(Frame(List(Roll(10))).score(Nil)._1 == 10)
      }
      "be a strike" in {
        assert(Frame(List(Roll(10))) isStrike)
      }
    }

    "has two rolls for 10" should {
      "have score 10" in {
        assert(Frame(List(Roll(7), Roll(3))).score(Nil)._1 == 10)
      }
      "be a spare" in {
        assert(Frame(List(Roll(7), Roll(3))) isSpare)
      }
    }

    "has a score less than 10" should {
      "not be a strike nor a spare" in {
        val frame1 = Frame(List(Roll(7)))
        val frame2 = Frame(List(Roll(7), Roll(2)))
        assert(!frame1.isSpare)
        assert(!frame1.isStrike)
        assert(!frame2.isSpare)
        assert(!frame2.isStrike)
      }
    }
    "have a score considering bonuses" in {
      val frame = Frame(List(Roll(7), Roll(2)))
      assert(frame.score(Nil)._1 == 9)
      assert(frame.score(List(1))._1 == 16)
      assert(frame.score(List(2))._1 == 18)
      assert(frame.score(List(1, 1))._1 == 23)
      assert(frame.score(List(2, 1))._1 == 25)
    }
  }

}
