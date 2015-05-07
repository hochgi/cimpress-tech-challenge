package com.github.hochgi.cimpress

import org.rogach.scallop.ScallopConf

import scala.concurrent.Await
import scala.concurrent.duration.Duration


object Main extends App {

  object Conf extends ScallopConf(args) {
    val loop = opt[Boolean](
      "loop",
      short='l',
      default = Some(false),
      descr = "run once, or enter loop mode")

    val mode = opt[String](
      "mode", short = 'm',
      default = Some("trial"),
      descr = "playing mode (must be trial or contest)",
      validate = Set("trial","contest").apply)

    val key = trailArg[String](
      "key",
      required = true,
      descr = "your private key")
  }

  val loop = Conf.loop.get.get
  var mode = Conf.mode.get.get
  val key = Conf.key.get.get
  var quit = false

  while(!quit && loop) {
    val puzzle = Http.getPuzzle(key,mode)
    val solution = Await.result(puzzle.map(Logic.solve),Duration.Inf) //TODO: Await indefinately?
    val score = Http.postSolution(solution, key)
    println(score + "\n")
    quit = scala.io.StdIn.readLine("type `quit` or exit to quit, any key to continue:").toLowerCase.matches("quit|exit")
  }
}