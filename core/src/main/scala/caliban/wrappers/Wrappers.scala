package caliban.wrappers

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.Value.NullValue
import caliban.execution.Field
import caliban.parsing.adt.Document
import caliban.wrappers.Wrapper.{ OverallWrapper, ValidationWrapper }
import caliban.{ GraphQLRequest, GraphQLResponse }
import zio.clock.Clock
import zio.console.{ putStrLn, putStrLnErr, Console }
import zio.duration._
import zio.{ Chunk, IO, UIO, URIO, ZIO }

import scala.annotation.tailrec

object Wrappers {

  /**
   * Returns a wrapper that prints errors to the console
   */
  lazy val printErrors: OverallWrapper[Console] =
    OverallWrapper { process => request =>
      process(request).tap(response =>
        ZIO.when(response.errors.nonEmpty)(
          putStrLnErr(response.errors.flatMap(prettyStackStrace).mkString("", "\n", "\n"))
        )
      )
    }

  private def prettyStackStrace(t: Throwable): Chunk[String] = {
    @tailrec def go(acc: Chunk[String], t: Throwable): Chunk[String] =
      if (t == null) acc
      else go(acc ++ (t.toString +: Chunk.fromArray(t.getStackTrace).map("\tat " + _.toString)), t.getCause)
    go(Chunk(""), t)
  }

  /**
   * Returns a wrapper that prints slow queries
   * @param duration threshold above which queries are considered slow
   */
  def printSlowQueries(duration: Duration): OverallWrapper[Console with Clock]                                  =
    onSlowQueries(duration) { case (time, query) => putStrLn(s"Slow query took ${time.render}:\n$query") }

  /**
   * Returns a wrapper that runs a given function in case of slow queries
   * @param duration threshold above which queries are considered slow
   */
  def onSlowQueries[R](duration: Duration)(f: (Duration, String) => URIO[R, Any]): OverallWrapper[R with Clock] =
    OverallWrapper { process => (request: GraphQLRequest) =>
      process(request).timed.flatMap { case (time, res) =>
        ZIO.when(time > duration)(f(time, request.query.getOrElse(""))).as(res)
      }
    }

  /**
   * Returns a wrapper that times out queries taking more than a specified time.
   * @param duration threshold above which queries should be timed out
   */
  def timeout(duration: Duration): OverallWrapper[Clock] =
    OverallWrapper { process => (request: GraphQLRequest) =>
      process(request)
        .timeout(duration)
        .map(
          _.getOrElse(
            GraphQLResponse(
              NullValue,
              List(
                ExecutionError(
                  s"Query was interrupted after timeout of ${duration.render}:\n${request.query.getOrElse("")}"
                )
              )
            )
          )
        )
    }

  /**
   * Returns a wrapper that checks that the query's depth is under a given max
   * @param maxDepth the max allowed depth
   */
  def maxDepth(maxDepth: Int): ValidationWrapper[Any] =
    ValidationWrapper { process => (doc: Document) =>
      for {
        req   <- process(doc)
        depth <- calculateDepth(req.field)
        _     <- IO.when(depth > maxDepth)(
                   IO.fail(ValidationError(s"Query is too deep: $depth. Max depth: $maxDepth.", ""))
                 )
      } yield req
    }

  private def calculateDepth(field: Field): UIO[Int] = {
    val self     = if (field.name.nonEmpty) 1 else 0
    val children = field.fields
    ZIO
      .foreach(children)(calculateDepth)
      .map {
        case Nil  => self
        case list => list.max + self
      }
  }

  /**
   * Returns a wrapper that checks that the query has a limited number of fields
   * @param maxFields the max allowed number of fields
   */
  def maxFields(maxFields: Int): ValidationWrapper[Any] =
    ValidationWrapper { process => (doc: Document) =>
      for {
        req    <- process(doc)
        fields <- countFields(req.field)
        _      <- IO.when(fields > maxFields)(
                    IO.fail(ValidationError(s"Query has too many fields: $fields. Max fields: $maxFields.", ""))
                  )
      } yield req
    }

  private def countFields(field: Field): UIO[Int] =
    innerFields(field.fields)

  private def innerFields(fields: List[Field]): UIO[Int] =
    IO.foreach(fields)(countFields).map(_.sum + fields.length)

}
