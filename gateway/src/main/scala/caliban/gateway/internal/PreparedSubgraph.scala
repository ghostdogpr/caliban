package caliban.gateway.internal

import caliban.gateway.Lookup
import caliban.parsing.adt.Document
import caliban.schema.RootType

private[gateway] final case class PreparedSubgraph(
  name: String,
  rootType: RootType,
  document: Document,
  federation: Boolean,
  lookups: List[Lookup],
  mapping: SchemaMapping
)
