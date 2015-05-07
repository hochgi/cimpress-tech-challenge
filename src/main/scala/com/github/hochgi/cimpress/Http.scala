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
          Puzzle(id, width, height, puzzle)
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

  implicit val system: ActorSystem = ActorSystem()
  implicit val timeout: Timeout = Timeout(15.seconds)
  val cimpress = "http://techchallenge.cimpress.com"


  def getPuzzle(key: String, mode: String = "trial"): Future[Puzzle] = {
    require(Set("trial","contest")(mode))
    val uri = cimpress + s"/$key/$mode/puzzle"


    //TODO: real code
    //    val resFut = (IO(spray.can.Http) ? HttpRequest(GET, Uri(uri))).mapTo[HttpResponse]

    //TODO: remove demo code
    val resFut = Future.successful(HttpResponse(
      StatusCodes.OK,
      HttpEntity("""{"width":31,"height":23,"puzzle":[[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,false,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,false,true,true,true,true,true,true,true,true,true,false,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,false],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,false,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,false,true],[true,true,true,true,true,true,true,true,true,false,true,true,false,true,true,true,false,true,true,true,false,true,true,true,true,true,true,true,true,true,true]],"id":"9ba603ec5844475daff91edd06b7726e-14310019118196075"}"""),
      Nil))
    //END OF DEMO CODE


    resFut.collect{
      case response if response.status.isSuccess => {
        val j = response.entity.asString
        //TODO: remove println
        println(s"\n$j\n")
        PuzzleJsonFormat.read(j.parseJson)
      }
    }
  }

  def postSolution(solution: Solution, key: String): String = {
    println(solution)
    """{"numberOfSquares": 2,"score": 0,"timePenalty": 0,"errors":[]}"""
  }
}
