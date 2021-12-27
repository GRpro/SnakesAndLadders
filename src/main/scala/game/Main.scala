package game

object Main extends App {

  val MaxSnakes = 20
  val MaxLadders = 20
  val Size = 10

  val Board = SquareBoardField.generate(Size, MaxLadders, MaxSnakes)
  val Solver = new Solver(Dice6)

  println("SNAKES & LADDERS")
  println(Board)

  val Solutions = Solver.solve(Board)
  val IsValid = Solutions.forall(_.isValidSolution(Board))
  println(s"Found ${Solutions.length} effective solutions, all valid ($IsValid)")

  Solutions.zipWithIndex.foreach { case (solution, i) =>
    println(s"Solution $i:")
    println(solution)
  }

}
