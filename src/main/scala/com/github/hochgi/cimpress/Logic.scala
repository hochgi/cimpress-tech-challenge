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
    println(puzzle)
    Solution(puzzle.id, Vector(Square(1,1,5)))
  }
}
