package caliban.modelgen

import caliban.parsing.adt.{ Document, Type }
import caliban.parsing.adt.Document._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition, TypeDefinition }
import caliban.parsing.adt.Type.{ FieldDefinition, ListType, NamedType }
import zio.IO

object Generator {
  def generate(doc: Document)(implicit writerContext: GQLWriterContext): String = writerContext.docWriter.write(doc)

  trait GQLWriter[A] {
    def write(a: A)(implicit context: GQLWriterContext): String
  }

  trait GQLWriterContext {
    implicit val fieldWriter: GQLWriter[FieldDefinition]
    implicit val typeWriter: GQLWriter[Type]
    implicit val typeDefWriter: GQLWriter[TypeDefinition]
    implicit val docWriter: GQLWriter[Document]
    implicit val queryWriter: GQLWriter[QueryDefinition]
    implicit val mutationWriter: GQLWriter[MutationDefinition]
    implicit val subscriptionWriter: GQLWriter[SubscriptionDefinition]
    implicit val fragmentWriter: GQLWriter[FragmentDefinition]
  }

  case class QueryDefinition(op: OperationDefinition)

  case class MutationDefinition(op: OperationDefinition)

  case class SubscriptionDefinition(op: OperationDefinition)

  object GQLWriter {
    def apply[A](implicit instance: GQLWriter[A]): GQLWriter[A] =
      instance
  }
}
