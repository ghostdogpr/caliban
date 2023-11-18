package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.Annotations.{ GQLInterface, GQLUnion }
import caliban.schema.DerivationUtils.*
import caliban.schema.Types.makeUnion
import magnolia1.TypeInfo

final private class SumSchema[R, A](
  _members: => (List[(String, __Type, List[Any])], List[Schema[R, Any]]),
  info: TypeInfo,
  annotations: List[Any]
)(ordinal: A => Int)
    extends Schema[R, A] {

  private lazy val (subTypes, schemas) = {
    val (m, s) = _members
    (m.sortBy(_._1), s.toVector)
  }

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
