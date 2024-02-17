package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.Annotations.{ GQLInterface, GQLOneOfInput, GQLUnion }
import caliban.schema.DerivationUtils.*
import caliban.schema.Types.makeUnion
import magnolia1.TypeInfo

import scala.annotation.threadUnsafe

final private class SumSchema[R, A](
  _members: => (List[(String, __Type, List[Any])], List[Schema[R, Any]]),
  info: TypeInfo,
  annotations: List[Any]
)(ordinal: A => Int)
    extends Schema[R, A] {

  @threadUnsafe
  private lazy val (subTypes, schemas) = {
    val (m, s) = _members
    (m.sortBy(_._1), s.toVector)
  }

  @threadUnsafe
  private lazy val isEnum = subTypes.forall((_, t, _) => t.allFields.isEmpty && t.allInputFields.isEmpty)

  private val isInterface  = annotations.exists(_.isInstanceOf[GQLInterface])
  private val isUnion      = annotations.contains(GQLUnion())
  private val isOneOfInput = annotations.contains(GQLOneOfInput())

  def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
    val _ = schemas
    if (!isInterface && !isUnion && subTypes.nonEmpty && isEnum && !isOneOfInput) mkEnum(annotations, info, subTypes)
    else if (isOneOfInput && isInput) mkOneOfInput(annotations, schemas.toList, info)
    else if (!isInterface)
      makeUnion(
        Some(getName(annotations, info)),
        getDescription(annotations),
        subTypes.map(_._2).distinctBy(_.name).map(fixEmptyUnionObject),
        Some(info.full),
        Some(getDirectives(annotations))
      )
    else {
      val impl = subTypes.map(_._2.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
      mkInterface(annotations, info, impl)
    }
  }

  def resolve(value: A): Step[R] = schemas(ordinal(value)).resolve(value)
}
