package game.solution

import game.{SquareBoardField, Field, Move, Path}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PathSpec extends AnyWordSpec with Matchers {

  private val field: Field = Field.fromCells(Array(1, 0, 0))

  "isValidSolution" should {
    "return valid when finished in last cell" in {
      Path(List(Move(SquareBoardField.InitialPosition, 1), Move(1, 1)))
        .isValidSolution(field) shouldBe true
    }

    "return valid when passed through last cell" in {
      Path(List(Move(SquareBoardField.InitialPosition, 1), Move(1, 2)))
        .isValidSolution(field) shouldBe true
    }

    "return invalid when haven't reached to finish" in {
      Path(List(Move(SquareBoardField.InitialPosition, 2)))
        .isValidSolution(field) shouldBe false
    }

    "return invalid when move positions don't match" in {
      Path(List(Move(SquareBoardField.InitialPosition, 1), Move(0, 2)))
        .isValidSolution(field) shouldBe false
    }

    "return invalid when no move detected" in {
      Path(List(Move(SquareBoardField.InitialPosition, 1), Move(1, 0)))
        .isValidSolution(field) shouldBe false
    }
  }
}
