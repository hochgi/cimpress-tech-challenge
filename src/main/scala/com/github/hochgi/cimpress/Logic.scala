package com.github.hochgi.cimpress

/**
 * Created by gilad on 5/6/15.
 */
object Logic {
  type Grid = Vector[Vector[Boolean]]
  case class Puzzle(id: String, width: Int, height: Int, puzzle: Grid, timeInMillis: Long)

  case class Square(x: Int, y: Int, size: Int) {
    def contain(i: Int,j: Int): Boolean = j >= x && j < (x + size) && i >= y && i < (y + size)
  }
  case class Solution(id: String,squares: Vector[Square])

  val limit = 7

  //TODO: A* like with time limit
  /**
   * g := #squares taken in cover / %grid that is covered
   * h := %grid that is uncovered / 100 millis passed
   */
  def solve(puzzle: Puzzle): Solution = puzzle match {
    case Puzzle(id, width, height, puzzle, timeInMillis) => {
      val maxSize = math.min(height, height)
      val xs = chooseTiling(width,height,maxSize,puzzle,timeInMillis)
      Solution(id,xs.toVector)
    }
  }

  def g(size: Int, grid: Grid, numOfSquares: Int) = {
    val covered = grid.flatMap(_.collect{case true => 1}).sum.toDouble / size
    covered * numOfSquares
  }

  def h(size : Int, grid: Grid, startTime: Long) = {
    val toCover = grid.flatMap(_.collect{case false => 1}).sum.toDouble / size
    val timePassed = (System.currentTimeMillis() - startTime) match {
      case t if t < 100 => 1.0
      case t => t / 100.0
    }
    toCover / timePassed
  }

  def chooseTiling(width: Int, height: Int, size: Int, grid: Grid, startTime: Long): List[Square] = size match {
    case 1 => {
      val xs = for {
        i <- 0 to (height - size)
        j <- 0 to (width - size)
        if grid(i)(j)
      } yield Square(j,i,1)
      xs.toList
    }
    case n => {
      validSquares(width,height,size,grid) match {
        case Nil => chooseTiling(width,height,size-1,grid,startTime)
        case squares => {
          val newSize =
            if(squares.size == 1) size - 1
            else size
          val sqr = squares(scala.util.Random.nextInt(squares.size))
          val grd = Vector.tabulate(height, width) { (i, j) =>
            if(sqr.contain(i,j)) false
            else grid(i)(j)
          }
          sqr :: chooseTiling(width, height, newSize, grd, startTime)
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
//      if counter < limit
      if range.forall(a => range.forall(b => grid(i+a)(j+b)))
    } yield {
      counter = counter + 1
      Square(j,i,size)
    }
  }

//  def solve(puzzle: Puzzle): Solution = {
//    val squares = for {
//      (row,i) <- puzzle.puzzle zip (0 until puzzle.height)
//      (sqr,j) <- row zip (0 until puzzle.width)
//      if sqr
//    } yield Square(j,i,1)
//    Solution(puzzle.id, squares)
//  }
//

//  def merge(j: Int, i: Int, squares: Seq[Square]) = {
//    squares.find{
//      case Square(x,y,size) =>
//    }
//    ???
//  }

//  def tileSquares(puzzle: Grid, squares: Vector[Square])(f: Grid => (Grid,Square)): Vector[Square] = {
//    if(puzzle.forall(_.forall(!_))) squares
//    else {
//      val (g,s) = f(puzzle)
//      tileSquares(g,squares :+ s)(f)
//    }
//  }
//
//  def chooseSquare(g: Grid, width: Int, height: Int, maxSize: Int, acc: List[Square] = Nil): (Grid,Seq[Square]) = maxSize match {
//    case 1 => emptyGrid(width, height) -> ((for {
//      (row,i) <- g zip (0 until height)
//      (sqr,j) <- row zip (0 until width)
//      if sqr
//    } yield Square(j,i,1)) ++ acc)
//    case n => for {
//      i <- (0 to (height - maxSize))
//      j <- (0 to (width - maxSize))
//      if g.drop(i).take(maxSize)
//    }
//  }
//
//  private def emptyGrid(width: Int, height: Int): Grid = (1 to height).map(_ => (1 to width).map(_ => false).toVector).toVector
}
