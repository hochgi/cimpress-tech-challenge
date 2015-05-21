package com.github.hochgi.cimpress

import com.github.hochgi.cimpress.Logic.{Solution, Puzzle}

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.util.Timeout
import akka.pattern.ask
import akka.io.IO

import spray.json._
import spray.httpx.SprayJsonSupport
import spray.http._ ,HttpMethods._


object Http extends SprayJsonSupport with AdditionalFormats {

  object JsonPuzzle extends DefaultJsonProtocol {

    implicit object PuzzleJsonFormat extends RootJsonReader[Puzzle] {

      override def read(value: JsValue): Puzzle = value match {
        case JsObject(xs) => {
          val id = xs("id") match {
            case JsString(v) => v
            case _ => deserializationError("id not string")
          }
          val width = xs("width") match {
            case JsNumber(n) if n.isValidInt => n.intValue
            case _ => deserializationError("width is not a valid int")
          }
          val height = xs("height") match {
            case JsNumber(n) if n.isValidInt => n.intValue
            case _ => deserializationError("height is not a valid int")
          }
          val puzzle = xs("puzzle") match {
            case JsArray(ys) if ys.size == height => ys map {
              case JsArray(zs) if zs.size == width => zs.map {
                case JsBoolean(b) => b
                case _ => deserializationError("value is not a valid boolean")
              }
              case _ => deserializationError("some inner row is not an array, or it's an array of invalid width")
            }
            case _ => deserializationError("puzzle isn't an array, or it's an array of invalid height")
          }
          Puzzle(id, width, height, puzzle, System.currentTimeMillis())
        }
        case _ => deserializationError("malformed format")
      }
    }

    implicit object SolutionJsonFormat extends RootJsonWriter[Solution] {
      override def write(solution: Solution): JsValue = JsObject(
        "id" -> JsString(solution.id),
        "squares" -> JsArray(
          solution.squares.map(
            square => JsObject(
              "X" -> JsNumber(square.x),
              "Y" -> JsNumber(square.y),
              "Size" -> JsNumber(square.size)
            )
          )
        )
      )
    }
  }

  import JsonPuzzle._
  import system.dispatcher // implicit execution context

  private implicit val system: ActorSystem = ActorSystem()
  private implicit val timeout: Timeout = Timeout(15.seconds)
  private val cimpress = "http://techchallenge.cimpress.com"

  def shutdown = system.shutdown()

  def getPuzzle(key: String, mode: String = "trial"): Future[Puzzle] = {
    require(Set("trial","contest")(mode))

    val uri = cimpress + s"/$key/$mode/puzzle"
    val resFut = (IO(spray.can.Http) ? HttpRequest(GET, Uri(uri))).mapTo[HttpResponse]

//    val resFut = Future.successful("""{"width":24,"height":23,"puzzle":[[false,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,false,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,false,true,true,true,true,true],[true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true],[true,false,true,true,true,true,true,true,true,true,true,false,true,true,true,false,true,true,true,true,false,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true],[true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false],[false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,false,true,true,true],[false,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,false,true,true,false,true,true],[true,true,true,true,false,true,true,true,false,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,false,true,false,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[false,false,true,false,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,false,false,false,true,false,true,true,false,true,true,true,true,true,true,false,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true],[true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true]],"id":"055a95950709461b883f042c58477d82-14313640330373557"}""")
    resFut.map{
//      _.parseJson.convertTo[Puzzle]
      case response if response.status.isSuccess => {
        val j = response.entity.asString.parseJson
        println(j.compactPrint + "\n")
        j.convertTo[Puzzle]
      }
    }
  }

  def postSolution(solution: Solution, key: String, mode: String = "trial"): Future[String] = {
    require(Set("trial","contest")(mode))

    val uri = cimpress + s"/$key/$mode/solution"
    val j = solution.toJson
    println(j.compactPrint + "\n")
    val body = HttpEntity(j.compactPrint)
    val resFut = (IO(spray.can.Http) ? HttpRequest(POST, Uri(uri), Nil, body)).mapTo[HttpResponse]

    resFut.collect{
      case response if response.status.isSuccess => {
        response.entity.asString
      }
    }
  }
}
