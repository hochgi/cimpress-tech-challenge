package com.github.hochgi.cimpress

import org.rogach.scallop.ScallopConf

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {

  object Conf extends ScallopConf(args) {
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

  var mode = Conf.mode.get.get
  val key = Conf.key.get.get
  var quit = false

  while(!quit) {
    val puzzle = Http.getPuzzle(key,mode)
    val solved = puzzle.map(Logic.solve)
    val future = solved.flatMap(Http.postSolution(_, key))
    val score = Await.result(future,Duration.Inf) //TODO: Await indefinately?
    println(score + "\n")
    val input = scala.io.StdIn.readLine("type `quit` or `exit` to quit, `c`/`contest` or `t`/`trial` to switch mode, ENTER to continue:").toLowerCase
    if(input.matches("c|contest")) mode = "contest"
    else if(input.matches("t|trial")) mode = "trial"
    quit = input.matches("quit|exit")
  }
  Http.shutdown
}
