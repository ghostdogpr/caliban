package caliban.parsing.adt

import caliban.parsing.SourceMapper
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition, TypeDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }

object Document {
  def typeDefinitions(doc: Document): List[TypeDefinition] = doc.definitions.collect { case td: TypeDefinition => td }
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
  def typeDefinition(doc: Document, name: String): Option[TypeDefinition] =
    typeDefinitions(doc).find(t => t.name == name)
}

case class Document(definitions: List[ExecutableDefinition], sourceMapper: SourceMapper)
