package game

import game.SquareBoardField.InitialPosition

import scala.annotation.tailrec

case class Path(moves: List[Move]) {

  def isValidSolution(gameField: Field): Boolean =
    if (moves.isEmpty) {
      false
    } else {
      val cellsArr = gameField.cells.toArray

      @tailrec
      def validate(position: Int, moves: List[Move]): Boolean = moves match {
        case Nil if position >= cellsArr.length -1 =>
          true
        case Nil =>
          false
        case mv :: lst =>
          if (position != mv.position) {
            false
          } else {
            var newPosition = position + mv.move
            if (newPosition >= cellsArr.length) {
              // passed through the finish
              true
            } else if (newPosition == position) {
              // no move happened, invalid
              false
            } else {
              while (cellsArr(newPosition) != 0) {
                newPosition = newPosition + cellsArr(newPosition)
              }
              validate(newPosition, lst)
            }
          }
      }

      validate(InitialPosition, moves)
    }

  override def toString: String =
    moves.map { mv =>
      if (mv.position == SquareBoardField.InitialPosition) {
        s"(Move ${mv.move}) Start -> ${mv.position + mv.move}"
      } else {
        s"(Move ${mv.move}) ${mv.position} -> ${mv.position + mv.move}"
      }
    }.mkString("\n")
}