package game.solution

import game.SquareBoardField
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SquareBoardFieldSpec extends AnyWordSpec with Matchers {

  private val attempts: Int = 1000

  "SquareBoardField" should {
    "fail creating when num of cells doesn't fit in square" in {
      assertThrows[IllegalArgumentException](SquareBoardField(Array.ofDim[Int](5)))
    }
  }

  "generate" should {

    "provide valid board with no transitive hops" in {
      (0 until attempts).foreach { _ =>
        val board = SquareBoardField.generate(10, 100, 100)
        println(board)

        board.ladders.foreach { ladder =>
          board.cells(ladder.start) should be > 0
          board.cells(ladder.start) shouldBe (ladder.end - ladder.start)
          board.cells(ladder.end) shouldBe 0
        }

        board.snakes.foreach { snake =>
          board.cells(snake.end) should be < 0
          board.cells(snake.end) shouldBe (snake.start - snake.end)
          board.cells(snake.start) shouldBe 0
        }
      }
    }

    "provide all ladders and snakes when size allows" in {
      (0 until attempts).foreach { _ =>
        val board = SquareBoardField.generate(4, 4, 3)
        println(board)

        board.snakes.size shouldBe 3
        board.ladders.size shouldBe 4
      }
    }
  }
}
