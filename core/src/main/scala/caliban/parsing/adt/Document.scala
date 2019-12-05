package caliban.parsing.adt

import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition, TypeDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }
import scala.reflect.ClassTag

object Document {
  def typeDefinitions      = definitions[TypeDefinition](_)
  def fragmentDefinitions  = definitions[FragmentDefinition](_)
  def operationDefinitions = definitions[OperationDefinition](_)
  def queryDefinitions =
    definitions[OperationDefinition](_: Document)
      .filter(q => q.operationType == Query)
  def mutationDefinitions =
    definitions[OperationDefinition](_: Document)
      .filter(q => q.operationType == Mutation)
  def subscriptionDefinitions =
    definitions[OperationDefinition](_: Document)
      .filter(q => q.operationType == Subscription)
  def typeDefinition(name: String) =
    definitions[TypeDefinition](_: Document)
      .find(t => t.name == name)

  def definitions[T <: ExecutableDefinition](doc: Document)(implicit tag: ClassTag[T]): List[T] =
    doc.definitions.flatMap {
      case t: T => List(t)
      case _    => List()
    }
}

case class Document(definitions: List[ExecutableDefinition])
