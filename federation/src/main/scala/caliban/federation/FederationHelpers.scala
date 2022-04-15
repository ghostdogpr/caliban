package caliban.federation

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value.{ NullValue, StringValue }
import caliban.introspection.adt.__InputValue
import caliban.schema.{ ArgBuilder, Schema, Types }

private[federation] object FederationHelpers {
  def traverseEither[A, B](list: List[Either[A, B]]): Either[A, List[B]] = {
    val iterator = list.iterator
    val result   = List.newBuilder[B]
    var error    = Option.empty[A]

    while (error.isEmpty && iterator.hasNext) {
      val b = iterator.next()
      b match {
        case Left(value)  =>
          result.clear()
          error = Some(value)
        case Right(value) => result += value
      }
    }

    error.toLeft(result.result())
  }

  private[federation] val _FieldSet = __InputValue(
    "fields",
    None,
    () => Types.makeScalar("_FieldSet"),
    None,
    None
  )

  case class _Any(__typename: String, fields: InputValue)

  implicit val anySchema: Schema[Any, _Any] =
    Schema.scalarSchema("_Any", None, None, _ => NullValue)

  val anyArgBuilder: ArgBuilder[_Any] = {
    case v @ InputValue.ObjectValue(fields) =>
      fields
        .get("__typename")
        .collect { case StringValue(__typename) =>
          _Any(__typename, v)
        }
        .toRight(ExecutionError("_Any must contain a __typename value"))
    case other                              => Left(ExecutionError(s"Can't build a _Any from input $other"))
  }

  case class RepresentationsArgs(representations: List[_Any])

  implicit val representationsArgBuilder: ArgBuilder[RepresentationsArgs] = {
    case InputValue.ObjectValue(fields) =>
      fields.get("representations").toRight(ExecutionError("_Any must contain a __typename value")).flatMap {
        case InputValue.ListValue(values) =>
          traverseEither(values.map(anyArgBuilder.build)).map(RepresentationsArgs.apply)
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
    fs => StringValue(fs.fields)
  )
}
