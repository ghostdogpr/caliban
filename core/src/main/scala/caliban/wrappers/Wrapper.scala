package caliban.wrappers

import caliban.CalibanError.{ ParsingError, ValidationError }
import caliban.execution.FieldInfo
import caliban.parsing.adt.Document
import caliban.{ CalibanError, GraphQLResponse, ResponseValue }
import zio.{ IO, ZIO }
import zquery.ZQuery

sealed trait Wrapper[-R]

object Wrapper {

  type WrappingFunction[-R, E, A, Info] = (IO[E, A], Info) => ZIO[R, E, A]

  case class OverallWrapper[-R](f: WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], String])
      extends Wrapper[R]
  case class ParsingWrapper[-R](f: WrappingFunction[R, ParsingError, Document, String])     extends Wrapper[R]
  case class ValidationWrapper[-R](f: WrappingFunction[R, ValidationError, Unit, Document]) extends Wrapper[R]
  case class ExecutionWrapper[-R](f: WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], Document])
      extends Wrapper[R]
  case class FieldWrapper[-R](
    f: (ZQuery[Any, Nothing, ResponseValue], FieldInfo) => ZQuery[R, CalibanError, ResponseValue],
    wrapPureValues: Boolean = false
  ) extends Wrapper[R]

  private[caliban] def wrap[R, E, A, Info](
    zio: IO[E, A]
  )(wrappers: List[WrappingFunction[R, E, A, Info]], info: Info): ZIO[R, E, A] = {
    def loop(zio: IO[E, A], wrappers: List[WrappingFunction[R, E, A, Info]]): ZIO[R, E, A] =
      wrappers match {
        case Nil => zio
        case wrapper :: tail =>
          ZIO.environment[R].flatMap(env => loop(wrapper(zio, info).provide(env), tail))
      }
    loop(zio, wrappers)
  }

  private[caliban] def decompose[R](wrappers: List[Wrapper[R]]): (
    List[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], String]],
    List[WrappingFunction[R, ParsingError, Document, String]],
    List[WrappingFunction[R, ValidationError, Unit, Document]],
    List[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], Document]],
    List[FieldWrapper[R]]
  ) =
    wrappers.foldLeft(
      (
        List.empty[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], String]],
        List.empty[WrappingFunction[R, ParsingError, Document, String]],
        List.empty[WrappingFunction[R, ValidationError, Unit, Document]],
        List.empty[WrappingFunction[R, Nothing, GraphQLResponse[CalibanError], Document]],
        List.empty[FieldWrapper[R]]
      )
    ) {
      case ((o, p, v, e, f), OverallWrapper(wrapper))    => (wrapper :: o, p, v, e, f)
      case ((o, p, v, e, f), ParsingWrapper(wrapper))    => (o, wrapper :: p, v, e, f)
      case ((o, p, v, e, f), ValidationWrapper(wrapper)) => (o, p, wrapper :: v, e, f)
      case ((o, p, v, e, f), ExecutionWrapper(wrapper))  => (o, p, v, wrapper :: e, f)
      case ((o, p, v, e, f), wrapper: FieldWrapper[_])   => (o, p, v, e, wrapper :: f)
    }

}
