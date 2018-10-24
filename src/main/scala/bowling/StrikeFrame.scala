package bowling

class StrikeFrame private (rolls: List[Roll]) extends Frame(rolls)

object StrikeFrame {
  def apply(rolls: List[Roll]): Option[StrikeFrame] = if(Frame(rolls).isStrike) Some(new StrikeFrame(rolls)) else None
}
