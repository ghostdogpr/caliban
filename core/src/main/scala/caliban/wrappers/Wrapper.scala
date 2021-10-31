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
import caliban.{ GraphQL, GraphQLAspect }

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
sealed trait Wrapper[-R] extends GraphQLAspect[Nothing, R] { self =>
  def |+|[R1 <: R](that: Wrapper[R1]): Wrapper[R1] = CombinedWrapper(List(self, that))

  def apply[R1 <: R](that: GraphQL[R1]): GraphQL[R1] =
    that.withWrapper(self)
}

object Wrapper {

  /**
   * `WrappingFunction[R, E, A, Info]` is an alias for a function that transforms an initial function
   * from `Info` to `ZIO[R, E, A]` into a new function from `Info` to `ZIO[R, E, A]`.
   */
  trait WrappingFunction[-R, E, A, Info] {
    def wrap[R1 <: R](f: Info => ZIO[R1, E, A]): Info => ZIO[R1, E, A]
  }

  sealed trait SimpleWrapper[-R, E, A, Info] extends Wrapper[R] {
    def wrap[R1 <: R](f: Info => ZIO[R1, E, A]): Info => ZIO[R1, E, A]
  }

  /**
   * Wrapper for the whole query processing.
   * Wraps a function from a request `GraphQLRequest` to a `URIO[R, GraphQLResponse[CalibanError]]`.
   */
  trait OverallWrapper[-R] extends SimpleWrapper[R, Nothing, GraphQLResponse[CalibanError], GraphQLRequest]

  /**
   * Wrapper for the query parsing stage.
   * Wraps a function from a query `String` to a `ZIO[R, ParsingError, Document]`.
   */
  trait ParsingWrapper[-R] extends SimpleWrapper[R, ParsingError, Document, String]

  /**
   * Wrapper for the query validation stage.
   * Wraps a function from a `Document` to a `ZIO[R, ValidationError, ExecutionRequest]`.
   */
  trait ValidationWrapper[-R] extends SimpleWrapper[R, ValidationError, ExecutionRequest, Document]

  /**
   * Wrapper for the query execution stage.
   * Wraps a function from an `ExecutionRequest` to a `URIO[R, GraphQLResponse[CalibanError]]`.
   */
  trait ExecutionWrapper[-R] extends SimpleWrapper[R, Nothing, GraphQLResponse[CalibanError], ExecutionRequest]

  /**
   * Wrapper for each individual field.
   * Takes a function from a `ZQuery[R, Nothing, ResponseValue]` and a `FieldInfo` and that returns a
   * `ZQuery[R, CalibanError, ResponseValue]`.
   * If `wrapPureValues` is true, every single field will be wrapped, which could have an impact on performances.
   * If false, simple pure values will be ignored.
   */
  abstract class FieldWrapper[-R](val wrapPureValues: Boolean = false) extends Wrapper[R] {
    def wrap[R1 <: R](
      query: ZQuery[R1, ExecutionError, ResponseValue],
      info: FieldInfo
    ): ZQuery[R1, ExecutionError, ResponseValue]
  }

  trait EffectWrappingFunction[-R]

  /**
   * Wrapper for the introspection query processing.
   * Takes a function from a `ZIO[R, ExecutionError, __Introspection]` and that returns a
   * `ZIO[R, ExecutionError, __Introspection]`.
   */
  trait IntrospectionWrapper[-R] extends Wrapper[R] {
    def wrap[R1 <: R](effect: ZIO[R1, ExecutionError, __Introspection]): ZIO[R1, ExecutionError, __Introspection]
  }

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
  )(wrappers: List[SimpleWrapper[R, E, A, Info]], info: Info): ZIO[R, E, A] = {
    @tailrec
    def loop(process: Info => ZIO[R, E, A], wrappers: List[SimpleWrapper[R, E, A, Info]]): Info => ZIO[R, E, A] =
      wrappers match {
        case Nil             => process
        case wrapper :: tail => loop(wrapper.wrap((info: Info) => process(info)), tail)
      }
    loop(process, wrappers)(info)
  }

  private[caliban] def decompose[R](wrappers: List[Wrapper[R]]): UIO[
    (
      List[OverallWrapper[R]],
      List[ParsingWrapper[R]],
      List[ValidationWrapper[R]],
      List[ExecutionWrapper[R]],
      List[FieldWrapper[R]],
      List[IntrospectionWrapper[R]]
    )
  ] =
    ZIO.foldLeft(wrappers)(
      (
        List.empty[OverallWrapper[R]],
        List.empty[ParsingWrapper[R]],
        List.empty[ValidationWrapper[R]],
        List.empty[ExecutionWrapper[R]],
        List.empty[FieldWrapper[R]],
        List.empty[IntrospectionWrapper[R]]
      )
    ) {
      case ((o, p, v, e, f, i), wrapper: OverallWrapper[R])       => UIO.succeed((wrapper :: o, p, v, e, f, i))
      case ((o, p, v, e, f, i), wrapper: ParsingWrapper[R])       => UIO.succeed((o, wrapper :: p, v, e, f, i))
      case ((o, p, v, e, f, i), wrapper: ValidationWrapper[R])    => UIO.succeed((o, p, wrapper :: v, e, f, i))
      case ((o, p, v, e, f, i), wrapper: ExecutionWrapper[R])     => UIO.succeed((o, p, v, wrapper :: e, f, i))
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
