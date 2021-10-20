package caliban.wrappers

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.Value.NullValue
import caliban.ResponseValue
import caliban.execution.{ ExecutionRequest, Field, FieldInfo }
import caliban.parsing.adt.{Document, Directive}
import caliban.wrappers.Wrapper.{ OverallWrapper, ValidationWrapper, FieldWrapper }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import zio.clock.Clock
import zio.console.{ putStrLn, putStrLnErr, Console }
import zio.duration._
import zio.{ Chunk, IO, UIO, URIO, ZIO }
import zio.query.ZQuery
import zio.stream.ZStream

import scala.annotation.tailrec
import caliban.ResponseValue.ListValue
import caliban.ResponseValue.ObjectValue
import caliban.ResponseValue.StreamValue
import caliban.ResponseValue.DeferValue
import caliban.Value
import zio.query.Described


object Wrappers {

  /**
   * Returns a wrapper that prints errors to the console
   */
  lazy val printErrors: OverallWrapper[Console] =
    new OverallWrapper[Console] {
      def wrap[R1 <: Console](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        request =>
          process(request).tap(response =>
            ZIO.when(response.errors.nonEmpty)(
              putStrLnErr(response.errors.flatMap(prettyStackStrace).mkString("", "\n", "\n")).orDie
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
  def printSlowQueries(duration: Duration): OverallWrapper[Console with Clock] =
    onSlowQueries(duration) { case (time, query) => putStrLn(s"Slow query took ${time.render}:\n$query").orDie }

  /**
   * Returns a wrapper that runs a given function in case of slow queries
   * @param duration threshold above which queries are considered slow
   */
  def onSlowQueries[R](duration: Duration)(f: (Duration, String) => URIO[R, Any]): OverallWrapper[R with Clock] =
    new OverallWrapper[R with Clock] {
      def wrap[R1 <: R with Clock](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          process(request).timed.flatMap { case (time, res) =>
            ZIO.when(time > duration)(f(time, request.query.getOrElse(""))).as(res)
          }
    }

  /**
   * Returns a wrapper that times out queries taking more than a specified time.
   * @param duration threshold above which queries should be timed out
   */
  def timeout(duration: Duration): OverallWrapper[Clock] =
    new OverallWrapper[Clock] {
      def wrap[R1 <: Clock](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
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
    new ValidationWrapper[Any] {
      def wrap[R1 <: Any](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
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
    new ValidationWrapper[Any] {
      def wrap[R1 <: Any](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
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

  lazy val deferredValues: FieldWrapper[Any] = 
    new FieldWrapper[Any](false) {
      def wrap[R1](
        query: ZQuery[R1, ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R1, ExecutionError, ResponseValue] = {
        // If this field is deferred we will push it off into a DeferredValue that wraps an existing query
        hasDeferred(info).fold(query) {
          d => ZQuery.environment[R1].map(env => ResponseValue.DeferValue(
            query.provide(Described(env, "resolve")),
            info.path,
            d.arguments.collectFirst {
              case ("label", Value.StringValue(s)) => s
            }.getOrElse("")
          ))
        }
      }
    }

  private def hasDeferred(field: FieldInfo): Option[Directive] =
    field.details.fragment.flatMap(_.directives.collectFirst {
      case d if d.name == "defer" => d
    })

  lazy val deferredExecution: Wrapper.ExecutionWrapper[Any] =
    new Wrapper.ExecutionWrapper[Any] {
      override def wrap[R1](f: ExecutionRequest => ZIO[R1,Nothing,GraphQLResponse[CalibanError]]): ExecutionRequest => ZIO[R1,Nothing,GraphQLResponse[CalibanError]] = 
        (request: ExecutionRequest) => ZIO.environment[R1].flatMap(env => f(request).map { response =>
          // Extract the deferred values from the response and push them into the extension field instead
          val (eagerResponse, deferred) = extractDeferredValues(response.data, Nil)

          val stream: ZStream[R1, CalibanError.ExecutionError, ObjectValue] = ZStream.fromIterable(deferred.groupBy {
            case (name, value) =>  value.label -> value.path
          }).mapMPar(16) {
            case ((label, path), values) =>
              ZQuery.foreachBatched(values) {
                case (name, DeferValue(v, _, _)) =>
                  v.map(name -> _) 
              }.map(f => ObjectValue(f)).run
          }

          response.copy(
            data = eagerResponse,
            extensions = Some(response.extensions.foldLeft(ObjectValue(List("__defer" -> StreamValue(stream.provide(env)))))(
              (s, acc) => ObjectValue(s.fields ++ acc.fields))
            )
          )
        })
    }

  type Path = List[Either[String, Int]]

  private def extractDeferredValues(value: ResponseValue, path: List[Either[String, Int]]): (ResponseValue, List[(String, DeferValue)]) = ???

}
