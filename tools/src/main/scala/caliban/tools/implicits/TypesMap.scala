package caliban.tools.implicits

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import io.estatico.newtype.macros.newtype

@newtype case class TypesMap(typesMap: Map[String, TypeDefinition])
