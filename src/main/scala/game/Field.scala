package game

case class Snake(start: Int, end: Int)
case class Ladder(start: Int, end: Int)


trait Field {
  def cells: Seq[Int]

  def ladders: Seq[Ladder] = cells
    .zipWithIndex
    .collect { case (e, idx) if e > 0 =>
      Ladder(idx, idx + e)
    }

  def snakes: Seq[Snake] = cells
    .zipWithIndex
    .collect { case (e, idx) if e < 0 =>
      Snake(idx + e, idx)
    }
}

object Field {
  def fromCells(fieldCells: Seq[Int]): Field = new Field {
    override val cells: Seq[Int] = fieldCells
  }
}