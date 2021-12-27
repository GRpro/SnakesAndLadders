package game

import scala.collection.mutable

case class State(cur: Int, moves: Seq[Move])

class Solver(val mover: Mover) {

  def solve(validBoard: Field): List[Path] = {

    val q: mutable.Queue[State] = mutable.Queue.empty[State]
    q.enqueue(State(-1, Seq.empty))

    val visited: Array[Boolean] = Array.fill(validBoard.cells.length)(false)
    var solutionFound = false

    var solution: Seq[List[Move]] = Seq.empty

    while (q.nonEmpty && !solutionFound) {
      val size = q.size

      (0 until size).foreach { _ =>
        val state = q.dequeue

        mover
          .possibleMoves
          .foreach { mv =>

            if (mv + state.cur < validBoard.cells.length) {
              var mvPos = state.cur + mv
              while (validBoard.cells(mvPos) != 0) {
                mvPos = mvPos + validBoard.cells(mvPos)
              }

              if (!visited(mvPos)) {

                if (mvPos >= validBoard.cells.length - 1) {
                  solutionFound = true
                  solution = solution :+ (state.moves :+ Move(state.cur, mv)).toList
                } else {
                  visited(mvPos) = true
                }
                q.enqueue(State(mvPos, state.moves :+ Move(state.cur, mv)))
              }
            } else {
              // passed last cell (finish), consider as valid solution
              solutionFound = true
              solution = solution :+ (state.moves :+ Move(state.cur, mv)).toList
            }
          }
      }
    }

    solution.map(Path).toList
  }
}
