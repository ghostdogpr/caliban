package caliban.schema

import caliban.introspection.adt.{ __Directive, __Type, __TypeKind }
import caliban.schema.Types.collectTypes

case class RootType(
  queryType: __Type,
  mutationType: Option[__Type],
  subscriptionType: Option[__Type],
  additionalTypes: List[__Type] = List.empty,
  additionalDirectives: List[__Directive] = List.empty
) {
  private val primitiveTypes: List[__Type] = List(
    __Type(kind = __TypeKind.SCALAR, name = Some("Boolean")),
    __Type(kind = __TypeKind.SCALAR, name = Some("Int")),
    __Type(kind = __TypeKind.SCALAR, name = Some("Float")),
    __Type(kind = __TypeKind.SCALAR, name = Some("String"))
  )

  val types: Map[String, __Type] = {
    val init = additionalTypes.foldLeft(List.empty[__Type]) { case (acc, t) => collectTypes(t, acc) }
    (init ++
      primitiveTypes ++
      collectTypes(queryType, init) ++
      mutationType.fold(List.empty[__Type])(collectTypes(_, init)) ++
      subscriptionType.fold(List.empty[__Type])(collectTypes(_, init)))
      .groupBy(t => (t.name, t.kind, t.origin))
      .flatMap(_._2.headOption)
      .map(t => t.name.getOrElse("") -> t)
      .toMap
  }
}
