package caliban.wrappers

import java.time.format.DateTimeFormatter
import java.time.{ Instant, ZoneId }
import java.util.concurrent.TimeUnit
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper, ParsingWrapper, ValidationWrapper }
import caliban.{ Rendering, ResponseValue }
import zio.{ clock, FiberRef }
import zio.clock.Clock
import zio.duration.Duration
import zquery.ZQuery

object ApolloTracing {

  /**
   * Returns a wrapper that adds tracing information to every response
   * following Apollo Tracing format: https://github.com/apollographql/apollo-tracing.
   */
  val apolloTracing: EffectfulWrapper[Clock] =
    EffectfulWrapper(
      FiberRef
        .make(Tracing())
        .map(
          ref =>
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
          "path" -> ListValue((Left(fieldName) :: path).reverse.map {
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
      ObjectValue(List("resolvers" -> ListValue(resolvers.sortBy(_.startOffset).map(_.toResponseValue))))
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

  private def apolloTracingOverall(ref: FiberRef[Tracing]): OverallWrapper[Clock] =
    OverallWrapper {
      case (io, _) =>
        for {
          nanoTime    <- clock.nanoTime
          currentTime <- clock.currentTime(TimeUnit.MILLISECONDS)
          _           <- ref.update(_.copy(startTime = currentTime, startTimeMonotonic = nanoTime))
          result <- io.timed.flatMap {
                     case (duration, result) =>
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

  private def apolloTracingParsing(ref: FiberRef[Tracing]): ParsingWrapper[Clock] =
    ParsingWrapper {
      case (io, _) =>
        for {
          start              <- clock.nanoTime
          (duration, result) <- io.timed
          _ <- ref.update(
                state =>
                  state.copy(
                    parsing = state.parsing.copy(startOffset = start - state.startTimeMonotonic, duration = duration)
                  )
              )
        } yield result
    }

  private def apolloTracingValidation(ref: FiberRef[Tracing]): ValidationWrapper[Clock] =
    ValidationWrapper {
      case (io, _) =>
        for {
          start              <- clock.nanoTime
          (duration, result) <- io.timed
          _ <- ref.update(
                state =>
                  state.copy(
                    validation =
                      state.validation.copy(startOffset = start - state.startTimeMonotonic, duration = duration)
                  )
              )
        } yield result
    }

  private def apolloTracingField(ref: FiberRef[Tracing]): FieldWrapper[Clock] =
    FieldWrapper(
      {
        case (query, fieldInfo) =>
          for {
            start    <- ZQuery.fromEffect(clock.nanoTime)
            result   <- query
            end      <- ZQuery.fromEffect(clock.nanoTime)
            duration = Duration.fromNanos(end - start)
            _ <- ZQuery.fromEffect(
                  ref
                    .update(
                      state =>
                        state.copy(
                          execution = state.execution.copy(
                            resolvers =
                              Resolver(
                                path = fieldInfo.path,
                                parentType = fieldInfo.parentType.fold("")(Rendering.renderTypeName),
                                fieldName = fieldInfo.fieldName,
                                returnType = Rendering.renderTypeName(fieldInfo.returnType),
                                startOffset = start - state.startTimeMonotonic,
                                duration = duration
                              ) :: state.execution.resolvers
                          )
                        )
                    )
                )
          } yield result
      },
      wrapPureValues = true
    )

}
