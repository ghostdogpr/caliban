package caliban.tools.implicits

import io.estatico.newtype.macros.newtype

@newtype case class ScalarMappings(scalarMap: Option[Map[String, String]])
