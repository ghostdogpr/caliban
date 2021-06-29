package caliban.wrappers

import java.time.format.DateTimeFormatter
import java.time.{ Instant, ZoneId }
import java.util.concurrent.TimeUnit
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.execution.{ ExecutionRequest, FieldInfo }
import caliban.parsing.adt.Document
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper, ParsingWrapper, ValidationWrapper }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, Rendering, ResponseValue }
import zio.{ clock, Ref, ZIO }
import zio.clock.Clock
import zio.duration.Duration
import zio.query.ZQuery

object ApolloTracing {

  /**
   * Returns a wrapper that adds tracing information to every response
   * following Apollo Tracing format: https://github.com/apollographql/apollo-tracing.
   */
  val apolloTracing: EffectfulWrapper[Clock] =
    EffectfulWrapper(
      Ref
        .make(Tracing())
        .map(ref =>
          apolloTracingOverall(ref) |+|
            apolloTracingParsing(ref) |+|
            apolloTracingValidation(ref) |+|
            apolloTracingField(ref)
        )
    )

  private val dateFormatter: DateTimeFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    .withZone(ZoneId.of("UTC"))

  case class Parsing(startOffset: Long = 0, duration: Duration = Duration.Zero) {
    def toResponseValue: ResponseValue =
      ObjectValue(List("startOffset" -> IntValue(startOffset), "duration" -> IntValue(duration.toNanos)))
  }

  case class Validation(startOffset: Long = 0, duration: Duration = Duration.Zero) {
    def toResponseValue: ResponseValue =
      ObjectValue(List("startOffset" -> IntValue(startOffset), "duration" -> IntValue(duration.toNanos)))
  }

  case class Resolver(
    path: List[Either[String, Int]] = Nil,
    parentType: String = "",
    fieldName: String = "",
    returnType: String = "",
    startOffset: Long = 0,
    duration: Duration = Duration.Zero
  ) {
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "path"        -> ListValue((Left(fieldName) :: path).reverse.map {
            case Left(s)  => StringValue(s)
            case Right(i) => IntValue(i)
          }),
          "parentType"  -> StringValue(parentType),
          "fieldName"   -> StringValue(fieldName),
          "returnType"  -> StringValue(returnType),
          "startOffset" -> IntValue(startOffset),
          "duration"    -> IntValue(duration.toNanos)
        )
      )
  }

  case class Execution(resolvers: List[Resolver] = Nil) {
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "resolvers" -> ListValue(resolvers.sortBy(r => (r.startOffset, r.duration.toNanos)).map(_.toResponseValue))
        )
      )
  }

  case class Tracing(
    version: Int = 1,
    startTime: Long = 0,
    endTime: Long = 0,
    startTimeMonotonic: Long = 0,
    duration: Duration = Duration.Zero,
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
          "duration"   -> IntValue(duration.toNanos),
          "parsing"    -> parsing.toResponseValue,
          "validation" -> validation.toResponseValue,
          "execution"  -> execution.toResponseValue
        )
      )
  }

  private def apolloTracingOverall(ref: Ref[Tracing]): OverallWrapper[Clock] =
    new OverallWrapper[Clock] {
      def wrap[R1 <: Clock](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          for {
            nanoTime    <- clock.nanoTime
            currentTime <- clock.currentTime(TimeUnit.MILLISECONDS)
            _           <- ref.update(_.copy(startTime = currentTime, startTimeMonotonic = nanoTime))
            result      <- process(request).timed.flatMap { case (duration, result) =>
                             for {
                               endTime <- clock.currentTime(TimeUnit.MILLISECONDS)
                               _       <- ref.update(_.copy(duration = duration, endTime = endTime))
                               tracing <- ref.get
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

  private def apolloTracingParsing(ref: Ref[Tracing]): ParsingWrapper[Clock] =
    new ParsingWrapper[Clock] {
      def wrap[R1 <: Clock](
        process: String => ZIO[R1, CalibanError.ParsingError, Document]
      ): String => ZIO[R1, CalibanError.ParsingError, Document] =
        (query: String) =>
          for {
            start              <- clock.nanoTime
            resultWithDuration <- process(query).timed
            (duration, result)  = resultWithDuration
            _                  <- ref.update(state =>
                                    state.copy(
                                      parsing = state.parsing.copy(startOffset = start - state.startTimeMonotonic, duration = duration)
                                    )
                                  )
          } yield result
    }

  private def apolloTracingValidation(ref: Ref[Tracing]): ValidationWrapper[Clock] =
    new ValidationWrapper[Clock] {
      def wrap[R1 <: Clock](
        process: Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest] =
        (doc: Document) =>
          for {
            start              <- clock.nanoTime
            resultWithDuration <- process(doc).timed
            (duration, result)  = resultWithDuration
            _                  <- ref.update(state =>
                                    state.copy(
                                      validation =
                                        state.validation.copy(startOffset = start - state.startTimeMonotonic, duration = duration)
                                    )
                                  )
          } yield result
    }

  private def apolloTracingField(ref: Ref[Tracing]): FieldWrapper[Clock] =
    new FieldWrapper[Clock](true) {
      def wrap[R1 <: Clock](
        query: ZQuery[R1, CalibanError.ExecutionError, ResponseValue],
        fieldInfo: FieldInfo
      ): ZQuery[R1, CalibanError.ExecutionError, ResponseValue] =
        for {
          summarized            <- query.summarized(clock.nanoTime)((_, _))
          ((start, end), result) = summarized
          duration               = Duration.fromNanos(end - start)
          _                     <- ZQuery.fromEffect(
                                     ref
                                       .update(state =>
                                         state.copy(
                                           execution = state.execution.copy(
                                             resolvers = Resolver(
                                               path = fieldInfo.path,
                                               parentType = fieldInfo.details.parentType.fold("")(Rendering.renderTypeName),
                                               fieldName = fieldInfo.name,
                                               returnType = Rendering.renderTypeName(fieldInfo.details.fieldType),
                                               startOffset = start - state.startTimeMonotonic,
                                               duration = duration
                                             ) :: state.execution.resolvers
                                           )
                                         )
                                       )
                                   )
        } yield result
    }

}
