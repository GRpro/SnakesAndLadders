package game

sealed trait Mover {
  def possibleMoves: List[Int]
}

object Dice6 extends Mover {
  override def possibleMoves: List[Int] =
    (1 to 6).toList
}

object Dice8 extends Mover {
  override def possibleMoves: List[Int] =
    (1 to 8).toList
}
