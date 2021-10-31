package caliban.tools.implicits

import io.estatico.newtype.macros.newtype

@newtype case class MappingClashedTypeNames(clashedTypesMap: Map[String, String])
