package bowling

import org.scalatest.WordSpec

class GameSpec extends WordSpec {

  "A Game" when {
    "empty" should {
      "have score 0" in {
        assert(Game(Nil).score == 0)
      }
    }

    "all rolls get 0" should {
      "have score 0" in {
        assert(Game(List.fill(10)(Frame(List(Roll(0), Roll(0))))).score == 0)
      }
    }

    "all rolls get strikes" should {
      "have score 300" in {
        assert(Game(List.fill(9)(Frame(List(Roll(10)))) :+ Frame(List.fill(3)(Roll(10)))).score == 300)
      }
    }

    "all rolls get 1" should {
      "have score 20" in {
        assert(Game(List.fill(10)(Frame(List(Roll(1), Roll(1))))).score == 20)
      }
    }

    "all rolls get 5" should {
      "have score 150" in {
        assert(Game(List.fill(9)(Frame(List(Roll(5), Roll(5)))) :+ Frame(List.fill(3)(Roll(5)))).score == 150)
      }
    }
  }

}
