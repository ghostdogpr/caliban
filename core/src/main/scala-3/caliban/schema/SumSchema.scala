package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.Annotations.{ GQLInterface, GQLUnion }
import caliban.schema.DerivationUtils.*
import caliban.schema.Types.makeUnion
import magnolia1.TypeInfo

final private class SumSchema[R, A](
  _members: => List[(String, List[Any], Schema[R, Any])],
  info: TypeInfo,
  annotations: List[Any]
)(ordinal: A => Int)
    extends Schema[R, A] {
  private lazy val members = _members

  private lazy val subTypes = members.map { (label, subTypeAnnotations, schema) =>
    (label, schema.toType_(), subTypeAnnotations)
  }.sortBy(_._1)

  private lazy val schemas = members.map(_._3).toVector // Vector's .apply is O(1) vs List's O(N)

  private lazy val isEnum = subTypes.forall((_, t, _) => t.allFields.isEmpty && t.allInputFields.isEmpty)
  private val isInterface = annotations.exists(_.isInstanceOf[GQLInterface])
  private val isUnion     = annotations.contains(GQLUnion())

  def toType(isInput: Boolean, isSubscription: Boolean): __Type =
    if (!isInterface && !isUnion && subTypes.nonEmpty && isEnum) mkEnum(annotations, info, subTypes)
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

  def resolve(value: A): Step[R] = schemas(ordinal(value)).resolve(value)
}
