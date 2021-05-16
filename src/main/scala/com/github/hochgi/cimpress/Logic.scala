package com.github.hochgi.cimpress

import java.io.FileWriter

import scala.collection.mutable

/**
 * Created by gilad on 5/6/15.
 */
object Logic {


  type Grid = Vector[Vector[Boolean]]

  implicit class GridOps(g: Grid) {
    def countUncovered = g.flatten.count(identity)
    def isCovered = g.forall(_.forall(!_))
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
      .find(_.exists(_._1))
      .flatMap(_.find(_._1).map(_._2))
    def squaresAt(i: Int, j: Int): Seq[Square] = {
      var rad = 0
      while((0 to rad).forall(k => g.isDefinedAt(i+rad) && g.isDefinedAt(i+k) && g(i+rad).isDefinedAt(j+k) && g(i+k).isDefinedAt(j+rad) && g(i+rad)(j+k) && g(i+k)(j+rad))) {
        rad += 1
      }
      (1 to rad).map(Square(j,i,_))
    }
    //TODO: optimization: binary search for the max size
    def maxSquareAt(i: Int, j: Int): Square = {
      var rad = 0
      while((0 to rad).forall(k => g.isDefinedAt(i+rad) && g.isDefinedAt(i+k) && g(i+rad).isDefinedAt(j+k) && g(i+k).isDefinedAt(j+rad) && g(i+rad)(j+k) && g(i+k)(j+rad))) {
        rad += 1
      }
      Square(j,i,rad)
    }
    def canCoverWith(square: Square): Boolean = {
      g.drop(square.y-1).take(square.size).flatMap(_.drop(square.x-1).take(square.size)).forall(identity)
//      square.coverCount(g) == square.size * square.size
    }
    def pretty(partialCover: Grid) = {
      require(g.size == partialCover.size && g.head.size == partialCover.head.size,"cover & orig differ in width/height")
      g.withIndex.map{
        _.map{
          case (false,_) => '#'
          case (_,(i,j)) if partialCover(i)(j) => '*'
          case _ => ' '
        }.mkString
      }.mkString("\n")
    }
    def pretty = {
      g.map{
        _.map{
          case false => '#'
          case true => ' '
        }.mkString
      }.mkString("\n")
    }
  }

  case class Puzzle(id: String, width: Int, height: Int, puzzle: Grid, timeInMillis: Long)

  case class Square(x: Int, y: Int, size: Int) {
    def contain(i: Int, j: Int): Boolean = j >= x && j < (x + size) && i >= y && i < (y + size)
    def coverCount(grid: Grid): Int = grid.drop(y).take(size).flatMap(_.drop(x).take(size)).count(identity)
  }

  case class Solution(id: String, squares: Vector[Square])

  trait Solver {
    def puzzle: Puzzle
    def solve: Solution
    def maxSquareSize = math.min(puzzle.width, puzzle.height)

    def validSquares(width: Int, height: Int, size: Int, grid: Grid): Seq[Square] = {
      val range = 0 until size
      for {
        i <- 0 to (height - size)
        j <- 0 to (width - size)
        if range.forall(a => range.forall(b => grid(i + a)(j + b)))
      } yield {
        Square(j, i, size)
      }
    }
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

    var queue: mutable.PriorityQueue[PartialSolution] = scala.collection.mutable.PriorityQueue.empty[PartialSolution]
    val grid = puzzle.puzzle

    def solve: Solution = puzzle match {
      case Puzzle(id, width, height, grid, _) => {
        grid.mostUpperLeftUncovered match {
          case None => Solution (id, Vector.empty[Square])
          case Some((i,j)) => {
            println(s"init: i=$i, j=$j")
            val squares = grid.squaresAt(i,j)
            squares.foreach{sqr =>
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
          g.squaresAt(i,j).foreach{ sqr =>
            queue.enqueue(PartialSolution(g.coverWith(sqr),squares :+ sqr))
          }
          val next = queue.dequeue()
          aStar(next)
        }
      }
    }
  }

  object SuperGreedyAStar {
    def apply(puzzle: Puzzle): SuperGreedyAStar = new SuperGreedyAStar(puzzle: Puzzle)
  }

  class SuperGreedyAStar(puzzle: Puzzle) extends AStar(puzzle) {
    lazy val superGreedyCover = {

      val fw = new FileWriter("./target/sg.out")
      fw.write("covering sg\n\n")

      def rec(acc: List[Square], uncovered: Grid, viableSquares: Set[Square]): List[Square] = {
        if(uncovered.isCovered) acc
        else {
          fw.append(grid.pretty(uncovered) + "\n\n")
          val sqr = viableSquares.maxBy(_.coverCount(uncovered))
          fw.append(s"going to cover with: $sqr\nwhich covers ${sqr.coverCount(uncovered)}\n\n")
          rec(sqr :: acc, uncovered.coverWith(sqr), viableSquares - sqr)
        }
      }
      val sgSeq = (for {
        i <- 0 until puzzle.height
        j <- 0 until puzzle.width
        if grid(i)(j)
        sqr = grid.maxSquareAt(i,j)
      } yield sqr).toSet
      println(s"sgSeq: ${sgSeq.size}")
      val rv = rec(Nil, grid, sgSeq)
      fw.flush()
      fw.close()
      println("super greedy: " + rv.size + "\n" + rv)
      rv
    }

    def h(covered: Grid, squares: Seq[Square]): Double = {
      val (wholes,partials) = superGreedyCover
        .filter(_.coverCount(covered) > 0)
        .partition(sqr => sqr.coverCount(covered) == sqr.size * sqr.size)
      val coveredWithPartials = (covered /: partials){
        case (g,s) => g.coverWith(s)
      }
      val partialsWhole: Double = (for{
        sqr <- partials
        i <- sqr.y to (sqr.y + sqr.size)
        j <- sqr.x to (sqr.x + sqr.size)
      } yield i -> j).toSet.size
      val uncoveredDiff: Double = covered.countUncovered - coveredWithPartials.countUncovered
      (uncoveredDiff / partialsWhole * partials.size) + wholes.size
    }

    def g(covered: Grid, squares: Seq[Square]): Double = squares.size
  }
}



