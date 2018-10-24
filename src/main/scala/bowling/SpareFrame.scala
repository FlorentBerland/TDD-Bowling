package bowling


class SpareFrame private (rolls: List[Roll]) extends Frame(rolls)

object SpareFrame {
  def apply(rolls: List[Roll]): Option[SpareFrame] = if(Frame(rolls).isSpare) Some(new SpareFrame(rolls)) else None
}
