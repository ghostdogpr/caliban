package caliban.federation

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.Value.{ NullValue, StringValue }
import caliban.introspection.adt.__InputValue
import caliban.schema.{ ArgBuilder, Schema, Types }

private[federation] object FederationHelpers {
  import caliban.syntax._

  private[federation] val _FieldSet = __InputValue(
    "fields",
    None,
    () => Types.makeScalar("_FieldSet"),
    None
  )

  case class _Any(__typename: String, fields: InputValue)

  object _Any {
    implicit val schema: Schema[Any, _Any] =
      Schema.scalarSchema("_Any", None, None, None, _ => NullValue)

    implicit val argBuilder: ArgBuilder[_Any] = {
      case v @ InputValue.ObjectValue(fields) =>
        fields.getOrElseNull("__typename") match {
          case StringValue(__typename) => Right(_Any(__typename, v))
          case _                       => Left(ExecutionError("_Any must contain a __typename value"))
        }
      case other                              => Left(ExecutionError(s"Can't build a _Any from input $other"))
    }
  }

  case class RepresentationsArgs(representations: List[_Any])

  object RepresentationsArgs {
    implicit val argBuilder: ArgBuilder[RepresentationsArgs] = {
      case InputValue.ObjectValue(fields) =>
        fields.getOrElseNull("representations") match {
          case InputValue.ListValue(values) => ArgBuilder.traverseInputList[_Any](values).map(RepresentationsArgs.apply)
          case null                         => Left(ExecutionError("RepresentationsArgs must contain a representations value"))
          case other                        => Left(ExecutionError(s"Can't build a representations from input $other"))
        }
      case other                          => Left(ExecutionError(s"Can't build a representations from input $other"))
    }
  }

  case class _Entity(__typename: String, value: InputValue)

  case class _Service(sdl: String)

  case class FieldSet(fields: String)

  object FieldSet {
    implicit val schema: Schema[Any, FieldSet] = Schema.scalarSchema[FieldSet](
      "_FieldSet",
      None,
      None,
      None,
      (fs: FieldSet) => StringValue(fs.fields)
    )
  }
}
