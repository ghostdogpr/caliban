package caliban.parsing.adt

import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ DirectiveDefinition, SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }

case class Document(definitions: List[Definition], sourceMapper: SourceMapper) {
  lazy val directiveDefinitions: List[DirectiveDefinition]             = definitions.collect { case dd: DirectiveDefinition =>
    dd
  }
  lazy val typeDefinitions: List[TypeDefinition]                       = definitions.collect { case td: TypeDefinition =>
    td
  }
  lazy val objectTypeDefinitions: List[ObjectTypeDefinition]           = definitions.collect { case td: ObjectTypeDefinition =>
    td
  }
  lazy val inputObjectTypeDefinitions: List[InputObjectTypeDefinition] = definitions.collect {
    case td: InputObjectTypeDefinition => td
  }
  lazy val interfaceTypeDefinitions: List[InterfaceTypeDefinition]     = definitions.collect {
    case td: InterfaceTypeDefinition => td
  }
  lazy val enumTypeDefinitions: List[EnumTypeDefinition]               = definitions.collect { case td: EnumTypeDefinition =>
    td
  }
  lazy val scalarTypeDefinitions: List[ScalarTypeDefinition]           = definitions.collect { case td: ScalarTypeDefinition =>
    td
  }
  lazy val unionTypeDefinitions: List[UnionTypeDefinition]             = definitions.collect { case td: UnionTypeDefinition =>
    td
  }
  lazy val fragmentDefinitions: List[FragmentDefinition]               = definitions.collect { case fd: FragmentDefinition =>
    fd
  }
  lazy val schemaDefinition: Option[SchemaDefinition]                  = definitions.collectFirst { case sd: SchemaDefinition =>
    sd
  }
  lazy val operationDefinitions: List[OperationDefinition]             = definitions.collect { case od: OperationDefinition =>
    od
  }
  lazy val queryDefinitions: List[OperationDefinition]                 =
    definitions.collect { case od: OperationDefinition => od }.filter(q => q.operationType == Query)
  lazy val mutationDefinitions: List[OperationDefinition]              =
    definitions.collect { case od: OperationDefinition => od }.filter(q => q.operationType == Mutation)
  lazy val subscriptionDefinitions: List[OperationDefinition]          =
    definitions.collect { case od: OperationDefinition => od }.filter(q => q.operationType == Subscription)
  def objectTypeDefinition(name: String): Option[ObjectTypeDefinition] =
    objectTypeDefinitions.find(t => t.name == name)
}
