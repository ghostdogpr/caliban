package caliban.modelgen

import caliban.parsing.adt.{Document, ExecutableDefinition, Selection, Type}
import caliban.parsing.adt.Type.{FieldDefinition, ListType, NamedType}
import Generator.GQLWriter
import Generator.GQLWriterContext
import Generator.QueryDefinition
import Generator.MutationDefinition
import Generator.SubscriptionDefinition
import caliban.parsing.adt.ExecutableDefinition.{FragmentDefinition, OperationDefinition, TypeDefinition}
import caliban.parsing.adt.Selection.{Field, FragmentSpread, InlineFragment}

object ScalaWriter {
  object DefaultGQLWriter extends ScalaGQLWriter
  trait ScalaGQLWriter extends GQLWriterContext {
    implicit val fieldWriter        = FieldWriter
    implicit val typeWriter         = TypeWriter
    implicit val typeDefWriter      = TypeDefinitionWriter
    implicit val docWriter          = DocumentWriter
    implicit val queryWriter        = QueryWriter
    implicit val mutationWriter     = MutationWriter
    implicit val subscriptionWriter = SubscriptionWriter
    implicit val fragmentWriter     = FragmentWriter
    implicit val selectionWriter    = SelectionTypeWriter
  }

  object DocumentWriter extends GQLWriter[Document] {
    override def write(doc: Document)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
      object Types {
        ${Document
        .typeDefinitions(doc)
        .map(GQLWriter[TypeDefinition].write(_)(schema))
        .mkString("\n\n")}
      }

      case class Queries(
        ${Document
        .queryDefinitions(doc)
        .map(QueryDefinition(_))
        .map(GQLWriter[QueryDefinition].write(_)(schema))
        .mkString("\n\n")}
      )

      object Mutations {
        ${Document
        .queryDefinitions(doc)
        .map(MutationDefinition(_))
        .map(GQLWriter[MutationDefinition].write(_)(schema))
        .mkString("\n\n")}
      }

      object Subscriptions {
        ${Document
        .queryDefinitions(doc)
        .map(SubscriptionDefinition(_))
        .map(GQLWriter[SubscriptionDefinition].write(_)(schema))
        .mkString("\n\n")}
      }

      object Fragments {
        ${Document
        .fragmentDefinitions(doc)
        .map(GQLWriter[FragmentDefinition].write(_)(schema))
        .mkString("\n\n")}
      }
      """
    }

  }

  object SelectionTypeWriter extends GQLWriter[Selection] {
    def dick: String = ""
    override def write(s: Selection)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s match {
        case i: InlineFragment => i.typeCondition.map(GQLWriter[Type].write(_)(schema)).getOrElse("Nothing")
        case f: FragmentSpread => f.name
        case f: Field => f.name
      }
    }
  }

  object FragmentWriter extends GQLWriter[FragmentDefinition] {
    override def write(a: FragmentDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      val gqlType = Document.typeDefinition(a.name)(schema)
      val selection = a.selectionSet.zip(a.selectionSet.map(GQLWriter[Selection].write(_)(schema))).map(_._2)
      s"""case class ${a.name}(${selection.mkString(",")})"""
    }
  }

  object SubscriptionWriter extends GQLWriter[SubscriptionDefinition] {
    override def write(a: SubscriptionDefinition)(schema: Document)(implicit context: GQLWriterContext): String = ???
  }

  object MutationWriter extends GQLWriter[MutationDefinition] {
    override def write(a: MutationDefinition)(schema: Document)(implicit context: GQLWriterContext): String = ???
  }

  object QueryWriter extends GQLWriter[QueryDefinition] {
    override def write(queryDef: QueryDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._
      val query = queryDef.op
      val queryName = query.name.getOrElse(s"UnnamedQuery")
      val queryArgs = query.variableDefinitions.map( v => GQLWriter[Type].write(v.variableType)(schema))
      val returnType = query.selectionSet.map( s => s )
      s"""${queryName}: (${queryArgs}) => """
    }
  }

  object TypeDefinitionWriter extends GQLWriter[TypeDefinition] {
    override def write(typedef: TypeDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""case class ${typedef.name}(${typedef.children.map(GQLWriter[FieldDefinition].write(_)(schema)).mkString(", ")})"""
    }
  }

  object FieldWriter extends GQLWriter[FieldDefinition] {
    override def write(f: FieldDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""${f.name}: ${GQLWriter[Type].write(f.ofType)(schema)}"""
    }
  }

  object TypeWriter extends GQLWriter[Type] {
    def convertType(name: String): String = name match {
      case _ => name
    }

    override def write(t: Type)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      t match {
        case NamedType(name, nonNull) if (nonNull)   => convertType(name)
        case NamedType(name, nonNull) if (!nonNull)  => s"Option[${convertType(name)}]"
        case ListType(ofType, nonNull) if (nonNull)  => s"List[${TypeWriter.write(ofType)(schema)}]"
        case ListType(ofType, nonNull) if (!nonNull) => s"Option[List[${TypeWriter.write(ofType)(schema)}]]"
      }
    }
  }

}
