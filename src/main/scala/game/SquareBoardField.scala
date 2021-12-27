package game

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class SquareBoardField(cells: Seq[Int]) extends Field {
  require(math.sqrt(cells.length) % 1 == 0, "Not a square board")

  override lazy val ladders: Seq[Ladder] = super.ladders
  override lazy val snakes: Seq[Snake] = super.snakes

  override def toString: String =
    if (cells.isEmpty) ""
    else {

      val size = cells.size
      val rowLen = math.sqrt(size).toInt

      val table: Seq[Seq[(Int, Int)]] = cells
        .zipWithIndex
        .grouped(rowLen)
        .toSeq
        .zipWithIndex
        .map { case (row, i) =>
          if (i % 2 == 1) row.reverse
          else row
        }
        .reverse

      val lastCellIdxLen = (size - 1).toString.length

      val cellIdxLen = lastCellIdxLen + "]".length
      val cellValueLen = 1 + lastCellIdxLen // 1 is reserved for sign + or -
      val colWidth = cellIdxLen + " ".length + cellValueLen + " ".length

      val rows = table.map {
        _
          .map { case (cell, idx) =>
            ("%-" + colWidth + "s").format(s"$idx] ${if (cell > 0) s"+$cell" else cell} ")
          }
          .mkString("|", "|", "|")
      }
      val separator = s"\n${(0 until rowLen).map(_ => "-" * colWidth).mkString("+", "+", "+")}\n"
      rows.mkString(separator, separator, separator)
    }
}

object SquareBoardField {

  val InitialPosition: Int = -1

  /**
    * Generates new pseudo-random Board of provided size with at most given number of leaders and snakes, without transitive hops
    *
    * @param size board size
    * @param maxLadders max num of [[Ladder]]s
    * @param maxSnakes max num of [[Snake]]s
    * @return [[SquareBoardField]]
    */
  def generate(size: Int,
               maxLadders: Int,
               maxSnakes: Int): SquareBoardField = {

    val board = Array.fill(size * size)(0)

    val free: ArrayBuffer[Int] = (0 until (size * size)).to(ArrayBuffer)

    val maxIt = math.max(maxSnakes, maxLadders)
    var cnt = 1
    while (cnt <= maxIt) {

      // exclude possibility for the last 2 cells to be snake
      if (cnt <= maxSnakes && free.size >= 3) {

        val startIdx = Random.nextInt(free.size - 2) // max = free.size - 3
        val start = free(startIdx)
        val endIdx = startIdx + 1 + Random.nextInt(free.size - startIdx - 2)
        val end = free(endIdx)

        board(end) = start - end

        free.remove(endIdx)
        free.remove(startIdx)
      }

      if (cnt <= maxLadders && free.size >= 2) {
        val startIdx = Random.nextInt(free.size - 1) // max = free.size - 2
        val start = free(startIdx)
        val endIdx = startIdx + 1 + Random.nextInt(free.size - startIdx - 1)
        val end = free(endIdx)

        board(start) = end - start

        free.remove(endIdx)
        free.remove(startIdx)
      }

      cnt += 1
    }

    SquareBoardField(board)
  }
}