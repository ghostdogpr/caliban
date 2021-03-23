package caliban.tools.implicits

import caliban.parsing.adt.Definition.TypeSystemDefinition

import scala.language.implicitConversions

object Implicits {
  implicit def typesMapToMap(typesMap: TypesMap): Map[String, TypeSystemDefinition.TypeDefinition] = typesMap.typesMap

  implicit def scalarMappingsToMap(typeMappings: ScalarMappings): Option[Map[String, String]] = typeMappings.scalarMap

  implicit def mappingClashedTypeNamesToMap(mappingClashedTypeNames: MappingClashedTypeNames): Map[String, String] =
    mappingClashedTypeNames.clashedTypesMap
}
