package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.Annotations.{ GQLInterface, GQLUnion }
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
  private lazy val (subTypes, schemas, emptyUnionObjectIdxs) = {
    val (m, s) = _members
    (
      m.sortBy(_._1),
      s.toVector,
      s.map(s0 => SchemaUtils.isEmptyUnionObject(s0.toType_())).toArray[Boolean]
    )
  }

  private var containsEmptyUnionObjects = false

  def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
    val _                         = schemas
    val isInterface               = annotations.exists(_.isInstanceOf[GQLInterface])
    val isUnion                   = annotations.contains(GQLUnion())
    @threadUnsafe lazy val isEnum = subTypes.forall((_, t, _) => t.allFields.isEmpty && t.allInputFields.isEmpty)

    if (!isInterface && !isUnion && subTypes.nonEmpty && isEnum) mkEnum(annotations, info, subTypes)
    else if (!isInterface) {
      containsEmptyUnionObjects = emptyUnionObjectIdxs.exists(identity)
      makeUnion(
        Some(getName(annotations, info)),
        getDescription(annotations),
        subTypes.map(_._2).distinctBy(_.name).map(SchemaUtils.fixEmptyUnionObject),
        Some(info.full),
        Some(getDirectives(annotations))
      )
    } else {
      val impl = subTypes.map(_._2.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
      mkInterface(annotations, info, impl)
    }
  }

  def resolve(value: A): Step[R] = {
    val idx  = ordinal(value)
    val step = schemas(idx).resolve(value)
    if (containsEmptyUnionObjects && emptyUnionObjectIdxs(idx))
      SchemaUtils.resolveEmptyUnionStep(step)
    else step
  }
}
