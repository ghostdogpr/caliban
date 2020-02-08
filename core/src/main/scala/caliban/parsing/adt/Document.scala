package caliban.parsing.adt

import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ EnumTypeDefinition, ObjectTypeDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }

object Document {
  def objectTypeDefinitions(doc: Document): List[ObjectTypeDefinition] = doc.definitions.collect {
    case td: ObjectTypeDefinition => td
  }
  def enumTypeDefinitions(doc: Document): List[EnumTypeDefinition] = doc.definitions.collect {
    case td: EnumTypeDefinition => td
  }
  def fragmentDefinitions(doc: Document): List[FragmentDefinition] = doc.definitions.collect {
    case fd: FragmentDefinition => fd
  }
  def operationDefinitions(doc: Document): List[OperationDefinition] = doc.definitions.collect {
    case od: OperationDefinition => od
  }
  def queryDefinitions(doc: Document): List[OperationDefinition] =
    doc.definitions.collect { case od: OperationDefinition => od }.filter(q => q.operationType == Query)
  def mutationDefinitions(doc: Document): List[OperationDefinition] =
    doc.definitions.collect { case od: OperationDefinition => od }.filter(q => q.operationType == Mutation)
  def subscriptionDefinitions(doc: Document): List[OperationDefinition] =
    doc.definitions.collect { case od: OperationDefinition => od }.filter(q => q.operationType == Subscription)
  def objectTypeDefinition(doc: Document, name: String): Option[ObjectTypeDefinition] =
    objectTypeDefinitions(doc).find(t => t.name == name)
}

case class Document(definitions: List[Definition], sourceMapper: SourceMapper)
