package game.solution

import game.{Dice6, Field, Move, Path, Solver}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SolverSpec extends AnyWordSpec with Matchers {

  "solve" should {

    "return empty solution" in {
      val solver = new Solver(Dice6)

      val board = Field.fromCells(Array(0, -1, -1, -1, -1, -1, -1, 0))

      solver.solve(board) shouldBe List.empty
    }

    "return all dice combinations as a solution for empty board" in {
      val solver = new Solver(Dice6)

      solver.solve(Field.fromCells(Array.empty[Int])) shouldBe Dice6
        .possibleMoves
        .map(mv => Path(List(Move(-1, mv))))
    }

    "return all dice combinations as a solution for one-sized board" in {
      val solver = new Solver(Dice6)

      solver.solve(Field.fromCells(Array(0))) shouldBe List(Path(List(Move(-1, 1)))) ::: Dice6
        .possibleMoves
        .drop(1)
        .map(mv => Path(List(Move(-1, mv))))
    }

    "return one solution by transitive hopping" in {
      val solver = new Solver(Dice6)

      solver.solve(Field.fromCells(Array(4, 2, -1, -3, 0))) should contain(Path(List(Move(-1, 3))))
    }

    "return many solutions by transitive hopping" in {
      val solver = new Solver(Dice6)

      val board = Field.fromCells(Array(0, 0, -2, 3, 2, -2, 0))

      solver.solve(board) shouldBe List(
        Path(List(Move(-1, 4))),
        Path(List(Move(-1, 5))),
        Path(List(Move(-1, 6)))
      )
    }
  }
}
