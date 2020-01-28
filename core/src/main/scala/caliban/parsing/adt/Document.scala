package caliban.parsing.adt

import caliban.parsing.SourceMapper

case class Document(definitions: List[ExecutableDefinition], sourceMapper: SourceMapper)
