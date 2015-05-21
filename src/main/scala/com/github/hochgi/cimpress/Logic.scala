package com.github.hochgi.cimpress

/**
 * Created by gilad on 5/6/15.
 */
object Logic {


  type Grid = Vector[Vector[Boolean]]

  implicit class GridOps(g: Grid) {
    def isCovered = g.forall(_.forall(!identity(_)))
    def withIndex = g.zipWithIndex.map{
      case (row,i) => row.zipWithIndex.map{
        case (b,j) => b -> (i,j)
      }
    }
    def coverWith(square: Square): Grid = Vector.tabulate(g.size, g.head.size) {
      (i, j) =>
        if (square.contain(i, j)) false
        else g(i)(j)
    }
    def mostUpperLeftUncovered: Option[(Int,Int)] = g
      .withIndex
      .find(_.exists(!_._1))
      .flatMap(_.find(!_._1).map(_._2))
    def squaresAt(i: Int, j: Int, sizeLimit: Int = math.min(g.size,g.head.size)): Seq[Square] = {
      var rad = 0
      while((0 to rad).forall(k => g(i+rad)(j+k) && g(i+k)(j+rad))) {
        rad += 1
      }
      (1 to rad).map(Square(i,j,_))
    }
  }

  case class Puzzle(id: String, width: Int, height: Int, puzzle: Grid, timeInMillis: Long)

  case class Square(x: Int, y: Int, size: Int) {
    def contain(i: Int, j: Int): Boolean = j >= x && j < (x + size) && i >= y && i < (y + size)
  }

  case class Solution(id: String, squares: Vector[Square])

  trait Solver {
    def puzzle: Puzzle
    def solve: Solution
    def maxSquareSize = math.min(puzzle.width, puzzle.height)
  }

  object Solver {
    def apply(puzzle: Puzzle, strategy: String = "RandomGreedy"): Solver = strategy match {
      case "RandomGreedy" =>  RandomGreedy(puzzle)
      case "SuperGreedyAStar" => SuperGreedyAStar(puzzle)
    }
  }

  case class RandomGreedy(puzzle: Puzzle) extends Solver {

    def solve: Solution = puzzle match {
      case Puzzle(id, width, height, puzzle, _) => {
        val xs = chooseTiling(width, height, maxSquareSize, puzzle)
        Solution(id, xs.toVector)
      }
    }

    def chooseTiling(width: Int, height: Int, size: Int, grid: Grid): List[Square] = size match {
      case 1 => {
        val xs = for {
          i <- 0 to (height - size)
          j <- 0 to (width - size)
          if grid(i)(j)
        } yield Square(j, i, 1)
        xs.toList
      }
      case n => {
        validSquares(width, height, size, grid) match {
          case Nil => chooseTiling(width, height, size - 1, grid)
          case squares => {
            val newSize =
              if (squares.size == 1) size - 1
              else size
            val sqr = squares(scala.util.Random.nextInt(squares.size))
            val grd = Vector.tabulate(height, width) { (i, j) =>
              if (sqr.contain(i, j)) false
              else grid(i)(j)
            }
            sqr :: chooseTiling(width, height, newSize, grd)
          }
        }
      }
    }

    def validSquares(width: Int, height: Int, size: Int, grid: Grid): Seq[Square] = {
      var counter = 0
      val range = 0 until size
      for {
        i <- 0 to (height - size)
        j <- 0 to (width - size)
        if range.forall(a => range.forall(b => grid(i + a)(j + b)))
      } yield {
        counter = counter + 1
        Square(j, i, size)
      }
    }
  }

  abstract class AStar(val puzzle: Puzzle) extends Solver {
    def h(covered: Grid, squares: Seq[Square]): Double
    def g(covered: Grid, squares: Seq[Square]): Double
    def f(covered: Grid, squares: Seq[Square]): Double = h(covered,squares) + g(covered,squares)

    case class PartialSolution(covered: Grid, squares: Seq[Square]) {
      def getSolution: Solution =
        if(covered.isCovered) Solution(puzzle.id, squares.toVector)
        else throw new NoSuchElementException("Partial solution is partial and cannot produce a full solution")
    }

    implicit object PartialSolutionOrdering extends Ordering[PartialSolution]{
      override def compare(x: PartialSolution, y: PartialSolution): Int = {
        val xRank = f(x.covered,x.squares)
        val yRank = f(y.covered,y.squares)

        if(xRank > yRank) scala.math.ceil(xRank - yRank).toInt
        else if(xRank < yRank) scala.math.ceil(yRank - xRank).toInt
        else 0
      }
    }

    val queue = scala.collection.mutable.PriorityQueue.empty[PartialSolution]
    val grid = puzzle.puzzle

    def solve: Solution = puzzle match {
      case Puzzle(id, width, height, grid, _) => {
        grid.mostUpperLeftUncovered match {
          case None => Solution (id, Vector.empty[Square])
          case Some((i,j)) => {
            grid.squaresAt(i,j,maxSquareSize).foreach{sqr =>
              queue.enqueue(PartialSolution(grid.coverWith(sqr), Seq(sqr)))
            }
            aStar(queue.dequeue())
          }
        }
      }
    }

    def aStar(ps: PartialSolution): Solution = ps match {
      case PartialSolution(g,squares) => g.mostUpperLeftUncovered match {
        case None => ps.getSolution
        case Some((i, j)) => {
          grid.squaresAt (i, j, maxSquareSize).foreach{ sqr =>
            queue.enqueue(PartialSolution(g.coverWith(sqr),squares :+ sqr))
          }
          aStar(queue.dequeue())
        }
      }
    }
  }

  object SuperGreedyAStar {
    def apply(puzzle: Puzzle): SuperGreedyAStar = new SuperGreedyAStar(puzzle: Puzzle)
  }

  class SuperGreedyAStar(puzzle: Puzzle) extends AStar(puzzle) {
//    val superGreedyCover = {
//      def mostCoverWithOverlapsSquares(): Seq[Square] = ???
//      def rec(acc: Seq[Square], uncovered: Grid): Seq[Square] = {
//        if(uncovered.isCovered) acc
//        else {
//          mostCoverWithOverlapsSquares() map {sqr =>
//            rec(acc :+ sqr, uncovered.coverWith(sqr))
//          }
//        }
//      }
//    }
    def h(covered: Grid, squares: Seq[Square]): Double = ???
    def g(covered: Grid, squares: Seq[Square]): Double = squares.size
  }
}



