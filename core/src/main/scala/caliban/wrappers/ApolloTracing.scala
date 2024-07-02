package caliban.wrappers

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban._
import caliban.execution.{ ExecutionRequest, FieldInfo }
import caliban.parsing.adt.Document
import caliban.wrappers.Wrapper._
import zio._
import zio.query.ZQuery

import java.time.format.DateTimeFormatter
import java.time.{ Instant, ZoneId }
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

object ApolloTracing {

  private val isEnabledRef = Unsafe.unsafe(implicit u => FiberRef.unsafe.make(true))

  /**
   * Returns a wrapper that adds tracing information to every response
   * following Apollo Tracing format: https://github.com/apollographql/apollo-tracing.
   *
   * @param excludePureFields Optionally disable tracing of pure fields.
   *                          Setting this to true can help improve performance at the cost of generating incomplete traces.
   *                          WARNING: Use this with caution as it could potentially cause issues if the tracing client expects all queried fields to be included in the traces
   * @see [[enabled]] and [[enabledWith]] to optionally control whether tracing is enabled for the current scope
   *      This can be used in combination with `HttpInterpreter.configure` from the `caliban-tapir` module or
   *      http middlewares to enable / disable tracing based on the request params (e.g., headers)
   */
  def apolloTracing(excludePureFields: Boolean = false): EffectfulWrapper[Any] =
    EffectfulWrapper(
      ZIO
        .whenZIO(isEnabledRef.get)(
          for {
            ref   <- ZIO.succeed(new AtomicReference(Tracing()))
            clock <- ZIO.clock
          } yield apolloTracingOverall(clock, ref) |+|
            apolloTracingParsing(clock, ref) |+|
            apolloTracingValidation(clock, ref) |+|
            apolloTracingField(clock.unsafe, ref, !excludePureFields)
        )
        .someOrElse(Wrapper.empty)
    )

  /**
   * Disable or enable tracing for the current scope
   */
  def enabled(value: Boolean): ZIO[Scope, Nothing, Unit] = isEnabledRef.locallyScoped(value)

  /**
   * Disable or enable tracing for the provided effect
   */
  def enabledWith[R, E, A](value: Boolean)(zio: ZIO[R, E, A]): ZIO[R, E, A] = isEnabledRef.locally(value)(zio)

  private val dateFormatter: DateTimeFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    .withZone(ZoneId.of("UTC"))

  case class Parsing(startOffset: Long = 0, durationNanos: Long = 0L) {
    def toResponseValue: ResponseValue =
      ObjectValue(List("startOffset" -> IntValue(startOffset), "duration" -> IntValue(durationNanos)))
  }

  case class Validation(startOffset: Long = 0, durationNanos: Long = 0L) {
    def toResponseValue: ResponseValue =
      ObjectValue(List("startOffset" -> IntValue(startOffset), "duration" -> IntValue(durationNanos)))
  }

  case class Resolver(
    path: List[PathValue] = Nil,
    parentType: String = "",
    fieldName: String = "",
    returnType: String = "",
    startOffset: Long = 0,
    durationNanos: Long = 0
  ) {
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "path"        -> ListValue((PathValue.Key(fieldName) :: path).reverse),
          "parentType"  -> StringValue(parentType),
          "fieldName"   -> StringValue(fieldName),
          "returnType"  -> StringValue(returnType),
          "startOffset" -> IntValue(startOffset),
          "duration"    -> IntValue(durationNanos)
        )
      )
  }

  object Resolver {
    implicit val ordering: Ordering[Resolver] = { (x: Resolver, y: Resolver) =>
      val ord1 = Ordering.Long.compare(x.startOffset, y.startOffset)
      if (ord1 != 0) ord1
      else Ordering.Long.compare(x.durationNanos, y.durationNanos)
    }
  }

  case class Execution(resolvers: List[Resolver] = Nil) {
    def toResponseValue: ResponseValue =
      ObjectValue(List("resolvers" -> ListValue(resolvers.sorted.map(_.toResponseValue))))
  }

  case class Tracing(
    version: Int = 1,
    startTime: Long = 0,
    endTime: Long = 0,
    startTimeMonotonic: Long = 0,
    durationNanos: Long = 0L,
    parsing: Parsing = Parsing(),
    validation: Validation = Validation(),
    execution: Execution = Execution()
  ) {
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "version"    -> IntValue(version),
          "startTime"  -> StringValue(dateFormatter.format(Instant.ofEpochMilli(startTime))),
          "endTime"    -> StringValue(dateFormatter.format(Instant.ofEpochMilli(endTime))),
          "duration"   -> IntValue(durationNanos),
          "parsing"    -> parsing.toResponseValue,
          "validation" -> validation.toResponseValue,
          "execution"  -> execution.toResponseValue
        )
      )
  }

  private def apolloTracingOverall(clock: Clock, ref: AtomicReference[Tracing]): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          for {
            nanoTime    <- clock.nanoTime
            currentTime <- clock.currentTime(TimeUnit.MILLISECONDS)
            _           <- ZIO.succeed(ref.updateAndGet(_.copy(startTime = currentTime, startTimeMonotonic = nanoTime)))
            result      <- process(request).timed.flatMap { case (duration, result) =>
                             for {
                               endTime <- clock.currentTime(TimeUnit.MILLISECONDS)
                               tracing <- ZIO.succeed(ref.get.copy(durationNanos = duration.toNanos, endTime = endTime))
                             } yield result.copy(
                               extensions = Some(
                                 ObjectValue(
                                   ("tracing" -> tracing.toResponseValue) ::
                                     result.extensions.fold(List.empty[(String, ResponseValue)])(_.fields)
                                 )
                               )
                             )
                           }
          } yield result
    }

  private def apolloTracingParsing(clock: Clock, ref: AtomicReference[Tracing]): ParsingWrapper[Any] =
    new ParsingWrapper[Any] {
      def wrap[R1](
        process: String => ZIO[R1, CalibanError.ParsingError, Document]
      ): String => ZIO[R1, CalibanError.ParsingError, Document] =
        (query: String) =>
          for {
            start              <- clock.nanoTime
            resultWithDuration <- process(query).timed
            (duration, result)  = resultWithDuration
            _                  <- ZIO.succeed(
                                    ref.updateAndGet(state =>
                                      state.copy(
                                        parsing = state.parsing
                                          .copy(startOffset = start - state.startTimeMonotonic, durationNanos = duration.toNanos)
                                      )
                                    )
                                  )
          } yield result
    }

  private def apolloTracingValidation(clock: Clock, ref: AtomicReference[Tracing]): ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      def wrap[R1](
        process: Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest] =
        (doc: Document) =>
          for {
            start              <- clock.nanoTime
            resultWithDuration <- process(doc).timed
            (duration, result)  = resultWithDuration
            _                  <-
              ZIO.succeed(
                ref.updateAndGet(state =>
                  state.copy(
                    validation = state.validation
                      .copy(startOffset = start - state.startTimeMonotonic, durationNanos = duration.toNanos)
                  )
                )
              )
          } yield result
    }

  private def apolloTracingField(
    clock: Clock#UnsafeAPI,
    ref: AtomicReference[Tracing],
    wrapPureValues: Boolean
  ): FieldWrapper[Any] =
    new FieldWrapper[Any](wrapPureValues) {
      import caliban.implicits.unsafe

      def wrap[R1](
        query: ZQuery[R1, CalibanError.ExecutionError, ResponseValue],
        fieldInfo: FieldInfo
      ): ZQuery[R1, CalibanError.ExecutionError, ResponseValue] =
        ZQuery.suspend {
          val start = clock.nanoTime()
          query.map { result =>
            val end = clock.nanoTime()
            val _   = ref.updateAndGet(state =>
              state.copy(
                execution = state.execution.copy(
                  resolvers = Resolver(
                    path = fieldInfo.path,
                    parentType = fieldInfo.details.parentType.fold("")(_.typeNameRepr),
                    fieldName = fieldInfo.name,
                    returnType = fieldInfo.details.fieldType.typeNameRepr,
                    startOffset = start - state.startTimeMonotonic,
                    durationNanos = end - start
                  ) :: state.execution.resolvers
                )
              )
            )
            result
          }
        }
    }

}
