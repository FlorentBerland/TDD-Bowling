package bowling

class RegularFrame private (rolls: List[Roll]) extends Frame(rolls)

object RegularFrame {
  def apply(rolls: List[Roll]): Option[RegularFrame] =
    if(Frame(rolls).isStrike || Frame(rolls).isSpare) None else Some(new RegularFrame(rolls))
}