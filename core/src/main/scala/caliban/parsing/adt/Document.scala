package caliban.parsing.adt

import caliban.parsing.adt.ExecutableDefinition.{FragmentDefinition, OperationDefinition, TypeDefinition}
import caliban.parsing.adt.OperationType.{Mutation, Query, Subscription}

object Document {
  def typeDefinitions         = definitions[TypeDefinition](_)
  def fragmentDefinitions     = definitions[FragmentDefinition](_)
  def operationDefinitions    = definitions[OperationDefinition](_)
  def queryDefinitions        = definitions[OperationDefinition](_)//.filter(_.operationType == Query)
  def mutationDefinitions     = definitions[OperationDefinition](_)//.filter(_.operationType == Mutation)
  def subscriptionDefinitions = definitions[OperationDefinition](_)//.filter(_.operationType == Subscription)

  def definitions[T <: ExecutableDefinition](doc: Document): List[T] =
    doc.definitions.flatMap {
      case t: T => List(t)
      case _    => List()
    }
}

case class Document(definitions: List[ExecutableDefinition])
