package caliban.federation

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value.{ NullValue, StringValue }
import caliban.introspection.adt.__InputValue
import caliban.schema.{ ArgBuilder, Schema, Types }

import scala.collection.mutable.ListBuffer

private[federation] object FederationHelpers {
  import caliban.syntax._

  private def traverseEither[A, B](list: List[Either[A, B]]): Either[A, List[B]] = {
    val result = ListBuffer.empty[B]
    var rem    = list

    while (rem ne Nil) {
      rem.head match {
        case Right(value) => result addOne value
        case l            => return l.asInstanceOf[Either[A, List[B]]]
      }
      rem = rem.tail
    }

    Right(result.result())
  }

  private[federation] val _FieldSet = __InputValue(
    "fields",
    None,
    () => Types.makeScalar("_FieldSet"),
    None
  )

  case class _Any(__typename: String, fields: InputValue)

  implicit val anySchema: Schema[Any, _Any] =
    Schema.scalarSchema("_Any", None, None, None, _ => NullValue)

  val anyArgBuilder: ArgBuilder[_Any] = { case v @ InputValue.ObjectValue(fields) =>
    fields.getOrElseNull("__typename") match {
      case StringValue(__typename) => Right(_Any(__typename, v))
      case null                    => Left(ExecutionError("_Any must contain a __typename value"))
      case other                   => Left(ExecutionError(s"Can't build a _Any from input $other"))
    }
  }

  case class RepresentationsArgs(representations: List[_Any])

  implicit val representationsArgBuilder: ArgBuilder[RepresentationsArgs] = {
    case InputValue.ObjectValue(fields) =>
      fields.getOrElseNull("representations") match {
        case InputValue.ListValue(values) =>
          traverseEither(values.map(anyArgBuilder.build)).map(RepresentationsArgs.apply)
        case null                         => Left(ExecutionError("_Any must contain a __typename value"))
        case other                        => Left(ExecutionError(s"Can't build a representations from input $other"))
      }
    case other                          => Left(ExecutionError(s"Can't build a representations from input $other"))
  }

  case class _Entity(__typename: String, value: InputValue)

  case class FieldSet(fields: String)
  case class _Service(sdl: String)

  implicit val fieldSetSchema: Schema[Any, FieldSet] = Schema.scalarSchema[FieldSet](
    "_FieldSet",
    None,
    None,
    None,
    (fs: FieldSet) => StringValue(fs.fields)
  )
}
