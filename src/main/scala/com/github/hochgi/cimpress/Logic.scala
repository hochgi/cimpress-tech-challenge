package com.github.hochgi.cimpress

/**
 * Created by gilad on 5/6/15.
 */
object Logic {
  type Grid = Vector[Vector[Boolean]]
  case class Puzzle(id: String, width: Int, height: Int, puzzle: Grid)

  case class Square(x: Int, y: Int, size: Int)
  case class Solution(id: String,squares: Vector[Square])

  def solve(puzzle: Puzzle): Solution = {
    val squares = for {
      (row,i) <- puzzle.puzzle zip (0 until puzzle.height)
      (sqr,j) <- row zip (0 until puzzle.width)
      if sqr
    } yield Square(j,i,1)
    Solution(puzzle.id, squares)
  }
}
