package caliban.tools.implicits

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition

case class TypesMap(typesMap: Map[String, TypeDefinition])
