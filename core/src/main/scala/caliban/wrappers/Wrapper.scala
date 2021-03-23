package caliban.wrappers

import scala.annotation.tailrec
import caliban.CalibanError.{ ExecutionError, ParsingError, ValidationError }
import caliban.execution.{ ExecutionRequest, FieldInfo }
import caliban.introspection.adt.__Introspection
import caliban.parsing.adt.Document
import caliban.wrappers.Wrapper.CombinedWrapper
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue }
import zio.{ UIO, ZIO }
import zio.query.ZQuery

/**
 * A `Wrapper[-R]` represents an extra layer of computation that can be applied on top of Caliban's query handling.
 * There are different base types of wrappers:
 * - `OverallWrapper` to wrap the whole query processing
 * - `ParsingWrapper` to wrap the query parsing only
 * - `ValidationWrapper` to wrap the query validation only
 * - `ExecutionWrapper` to wrap the query execution only
 * - `FieldWrapper` to wrap each field execution
 *
 * It is also possible to combine wrappers using `|+|` and to build a wrapper effectfully with `EffectfulWrapper`.
 */
sealed trait Wrapper[-R] { self =>
  def |+|[R1 <: R](that: Wrapper[R1]): Wrapper[R1] = CombinedWrapper(List(self, that))
}

object Wrapper {

  /**
   * `WrappingFunction[R, E, A, Info]` is an alias for a function that transforms an initial function
   * from `Info` to `ZIO[R, E, A]` into a new function from `Info` to `ZIO[R, E, A]`.
   */
  type WrappingFunction[R, E, A, Info] = (Info => ZIO[R, E, A]) => Info => ZIO[R, E, A]

  /**
   * Wrapper for the whole query processing.
   * Wraps a function from a request `GraphQLRequest` to a `URIO[R, GraphQLResponse[CalibanError]]`.
   */
  case class OverallWrapper[R](f: WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], GraphQLRequest])
      extends Wrapper[R]

  /**
   * Wrapper for the query parsing stage.
   * Wraps a function from a query `String` to a `ZIO[R, ParsingError, Document]`.
   */
  case class ParsingWrapper[R](f: WrappingFunction[R, ParsingError, Document, String]) extends Wrapper[R]

  /**
   * Wrapper for the query validation stage.
   * Wraps a function from a `Document` to a `ZIO[R, ValidationError, ExecutionRequest]`.
   */
  case class ValidationWrapper[R](f: WrappingFunction[R, ValidationError, ExecutionRequest, Document])
      extends Wrapper[R]

  /**
   * Wrapper for the query execution stage.
   * Wraps a function from an `ExecutionRequest` to a `URIO[R, GraphQLResponse[CalibanError]]`.
   */
  case class ExecutionWrapper[R](f: WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], ExecutionRequest])
      extends Wrapper[R]

  /**
   * Wrapper for each individual field.
   * Takes a function from a `ZQuery[R, Nothing, ResponseValue]` and a `FieldInfo` and that returns a
   * `ZQuery[R, CalibanError, ResponseValue]`.
   * If `wrapPureValues` is true, every single field will be wrapped, which could have an impact on performances.
   * If false, simple pure values will be ignored.
   */
  case class FieldWrapper[R](
    f: (ZQuery[R, ExecutionError, ResponseValue], FieldInfo) => ZQuery[R, ExecutionError, ResponseValue],
    wrapPureValues: Boolean = false
  ) extends Wrapper[R]

  /**
   * Wrapper for the introspection query processing.
   * Takes a function from a `ZIO[R, ExecutionError, __Introspection]` and that returns a
   * `ZIO[R, ExecutionError, __Introspection]`.
   */
  case class IntrospectionWrapper[R](
    f: ZIO[R, ExecutionError, __Introspection] => ZIO[R, ExecutionError, __Introspection]
  ) extends Wrapper[R]

  /**
   * Wrapper that combines multiple wrappers.
   * @param wrappers a list of wrappers
   */
  case class CombinedWrapper[-R](wrappers: List[Wrapper[R]]) extends Wrapper[R] {
    override def |+|[R1 <: R](that: Wrapper[R1]): Wrapper[R1] = that match {
      case CombinedWrapper(other) => copy(wrappers = wrappers ++ other)
      case other                  => copy(wrappers = wrappers :+ other)
    }

  }

  /**
   * A wrapper that requires an effect to be built. The effect will be run for each query.
   * @param wrapper an effect that builds a wrapper
   */
  case class EffectfulWrapper[-R](wrapper: UIO[Wrapper[R]]) extends Wrapper[R]

  private[caliban] def wrap[R1 >: R, R, E, A, Info](
    process: Info => ZIO[R1, E, A]
  )(wrappers: List[WrappingFunction[R, E, A, Info]], info: Info): ZIO[R, E, A] = {
    @tailrec
    def loop(process: Info => ZIO[R, E, A], wrappers: List[WrappingFunction[R, E, A, Info]]): Info => ZIO[R, E, A] =
      wrappers match {
        case Nil             => process
        case wrapper :: tail => loop(wrapper((info: Info) => process(info)), tail)
      }
    loop(process, wrappers)(info)
  }

  private[caliban] def decompose[R](wrappers: List[Wrapper[R]]): UIO[
    (
      List[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], GraphQLRequest]],
      List[WrappingFunction[R, ParsingError, Document, String]],
      List[WrappingFunction[R, ValidationError, ExecutionRequest, Document]],
      List[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], ExecutionRequest]],
      List[FieldWrapper[R]],
      List[IntrospectionWrapper[R]]
    )
  ] =
    ZIO.foldLeft(wrappers)(
      (
        List.empty[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], GraphQLRequest]],
        List.empty[WrappingFunction[R, ParsingError, Document, String]],
        List.empty[WrappingFunction[R, ValidationError, ExecutionRequest, Document]],
        List.empty[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], ExecutionRequest]],
        List.empty[FieldWrapper[R]],
        List.empty[IntrospectionWrapper[R]]
      )
    ) {
      case ((o, p, v, e, f, i), wrapper: OverallWrapper[R])       => UIO.succeed((wrapper.f :: o, p, v, e, f, i))
      case ((o, p, v, e, f, i), wrapper: ParsingWrapper[R])       => UIO.succeed((o, wrapper.f :: p, v, e, f, i))
      case ((o, p, v, e, f, i), wrapper: ValidationWrapper[R])    => UIO.succeed((o, p, wrapper.f :: v, e, f, i))
      case ((o, p, v, e, f, i), wrapper: ExecutionWrapper[R])     => UIO.succeed((o, p, v, wrapper.f :: e, f, i))
      case ((o, p, v, e, f, i), wrapper: FieldWrapper[R])         => UIO.succeed((o, p, v, e, wrapper :: f, i))
      case ((o, p, v, e, f, i), wrapper: IntrospectionWrapper[R]) => UIO.succeed((o, p, v, e, f, wrapper :: i))
      case ((o, p, v, e, f, i), CombinedWrapper(wrappers))        =>
        decompose(wrappers).map { case (o2, p2, v2, e2, f2, i2) =>
          (o2 ++ o, p2 ++ p, v2 ++ v, e2 ++ e, f2 ++ f, i2 ++ i)
        }
      case ((o, p, v, e, f, i), EffectfulWrapper(wrapper))        =>
        wrapper.flatMap(w =>
          decompose(List(w)).map { case (o2, p2, v2, e2, f2, i2) =>
            (o2 ++ o, p2 ++ p, v2 ++ v, e2 ++ e, f2 ++ f, i2 ++ i)
          }
        )
    }

}
