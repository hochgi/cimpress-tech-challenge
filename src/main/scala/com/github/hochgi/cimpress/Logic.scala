package com.github.hochgi.cimpress

/**
 * Created by gilad on 5/6/15.
 */
object Logic {
  type Grid = Vector[Vector[Boolean]]
  case class Puzzle(id: String, width: Int, height: Int, puzzle: Grid)

  type Solution = Any //TODO...

  def solve(puzzle: Puzzle): Solution = {
    ???
  }
}
