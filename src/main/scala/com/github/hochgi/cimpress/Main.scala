package com.github.hochgi.cimpress

import org.rogach.scallop.ScallopConf

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

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

  while(!quit) {
    val puzzle = Http.getPuzzle(key,mode)
    val future = puzzle.map(Logic.solve).flatMap(Http.postSolution(_, key))
    val score = Await.result(future,Duration.Inf) //TODO: Await indefinately?
    println(score + "\n")
    quit = !loop && scala.io.StdIn.readLine("type `quit` or `exit` to quit, ENTER to continue:").toLowerCase.matches("quit|exit")
  }
  Http.shutdown
}
