package caliban.modelgen

import caliban.parsing.adt.{ Document, ExecutableDefinition, Type }
import caliban.parsing.adt.Type.{ FieldDefinition, ListType, NamedType }
import Generator.GQLWriter
import Generator.GQLWriterContext
import Generator.QueryDefinition
import Generator.MutationDefinition
import Generator.SubscriptionDefinition
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition, TypeDefinition }

object ScalaWriter {
  implicit object DefaultGQLWriter extends GQLWriterContext {
    implicit val fieldWriter        = FieldWriter
    implicit val typeWriter         = TypeWriter
    implicit val typeDefWriter      = TypeDefinitionWriter
    implicit val docWriter          = DocumentWriter
    implicit val queryWriter        = QueryWriter
    implicit val mutationWriter     = MutationWriter
    implicit val subscriptionWriter = SubscriptionWriter
    implicit val fragmentWriter     = FragmentWriter
  }

  object DocumentWriter extends GQLWriter[Document] {
    override def write(doc: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
      object Types {
        ${Document
        .typeDefinitions(doc)
        .map(GQLWriter[TypeDefinition].write(_))
        .mkString("\n\n")}
      }

      object Queries {
        ${Document
        .queryDefinitions(doc)
        .map(QueryDefinition(_))
        .map(GQLWriter[QueryDefinition].write(_))
        .mkString("\n\n")}

      object Mutations {
        ${Document
        .queryDefinitions(doc)
        .map(MutationDefinition(_))
        .map(GQLWriter[MutationDefinition].write(_))
        .mkString("\n\n")}

      object Subscriptions {
        ${Document
        .queryDefinitions(doc)
        .map(SubscriptionDefinition(_))
        .map(GQLWriter[SubscriptionDefinition].write(_))
        .mkString("\n\n")}

      object Fragments {
        ${Document
        .fragmentDefinitions(doc)
        .map(GQLWriter[FragmentDefinition].write(_))
        .mkString("\n\n")}
      """
    }

  }

  object FragmentWriter extends GQLWriter[FragmentDefinition] {
    override def write(a: FragmentDefinition)(implicit context: GQLWriterContext): String = ???
  }

  object SubscriptionWriter extends GQLWriter[SubscriptionDefinition] {
    override def write(a: SubscriptionDefinition)(implicit context: GQLWriterContext): String = ???
  }

  object MutationWriter extends GQLWriter[MutationDefinition] {
    override def write(a: MutationDefinition)(implicit context: GQLWriterContext): String = ???
  }

  object QueryWriter extends GQLWriter[QueryDefinition] {
    override def write(a: QueryDefinition)(implicit context: GQLWriterContext): String = ???
  }

  object TypeDefinitionWriter extends GQLWriter[TypeDefinition] {
    override def write(typedef: TypeDefinition)(implicit context: GQLWriterContext): String = {
      import context._

      s"""case class ${typedef.name}(${typedef.children.map(GQLWriter[FieldDefinition].write(_)).mkString(", ")})"""
    }
  }

  object FieldWriter extends GQLWriter[FieldDefinition] {
    override def write(f: FieldDefinition)(implicit context: GQLWriterContext): String = {
      import context._

      s"""${f.name}: ${GQLWriter[Type].write(f.ofType)}"""
    }
  }

  object TypeWriter extends GQLWriter[Type] {
    def convertType(name: String): String = name match {
      case _ => name
    }

    override def write(t: Type)(implicit context: GQLWriterContext): String = {
      import context._

      t match {
        case NamedType(name, nonNull) if (nonNull)   => convertType(name)
        case NamedType(name, nonNull) if (!nonNull)  => s"Option[${convertType(name)}]"
        case ListType(ofType, nonNull) if (nonNull)  => s"List[${TypeWriter.write(ofType)}]"
        case ListType(ofType, nonNull) if (!nonNull) => s"Option[List[${TypeWriter.write(ofType)}]]"
      }
    }
  }

}
