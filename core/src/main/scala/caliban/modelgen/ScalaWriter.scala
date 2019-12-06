package caliban.modelgen

import caliban.parsing.adt.{Document, ExecutableDefinition, Selection, Type}
import caliban.parsing.adt.Type.{FieldDefinition, ListType, NamedType}
import Generator.{GQLWriter, GQLWriterContext, MutationArgs, MutationDef, QueryArgs, QueryDef, RootMutationDef, RootQueryDef, RootSubscriptionDef, SubscriptionArgs, SubscriptionDef, SubscriptionDefinition}
import caliban.parsing.adt.ExecutableDefinition.{FragmentDefinition, OperationDefinition, TypeDefinition}
import caliban.parsing.adt.Selection.{Field, FragmentSpread, InlineFragment}

object ScalaWriter {
  trait ScalaGQLWriter extends GQLWriterContext {
    implicit val fieldWriter            = FieldWriter
    implicit val typeWriter             = TypeWriter
    implicit val typeDefWriter          = TypeDefinitionWriter
    implicit val docWriter              = DocumentWriter
    implicit val rootQueryWriter        = RootQueryDefWriter
    implicit val queryWriter            = QueryDefWriter
    implicit val queryArgsWriter        = QueryArgsWriter
    implicit val rootMutationWriter     = RootMutationDefWriter
    implicit val mutationWriter         = MutationDefWriter
    implicit val mutationArgsWriter     = MutationArgsWriter
    implicit val rootSubscriptionWriter = RootSubscriptionDefWriter
    implicit val subscriptionWriter     = SubscriptionDefWriter
    implicit val subscriptionArgsWriter = SubscriptionArgsWriter
    implicit val fragmentWriter         = FragmentWriter
    implicit val selectionWriter        = SelectionTypeWriter
  }

  object DefaultGQLWriter extends ScalaGQLWriter

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

      ${Document.typeDefinition("Query")(schema).map(t => GQLWriter[RootQueryDef].write(RootQueryDef(t))(schema))}

      ${Document
        .typeDefinition("Mutation")(schema)
        .map(t => GQLWriter[RootMutationDef].write(RootMutationDef(t))(schema))}

      ${Document
        .typeDefinition("Subscription")(schema)
        .map(t => GQLWriter[RootSubscriptionDef].write(RootSubscriptionDef(t))(schema))}

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
    override def write(s: Selection)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s match {
        case i: InlineFragment => i.typeCondition.map(GQLWriter[Type].write(_)(schema)).getOrElse("Nothing")
        case f: FragmentSpread => f.name
        case f: Field          => f.name
      }
    }
  }

  object FragmentWriter extends GQLWriter[FragmentDefinition] {
    override def write(a: FragmentDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      val gqlType = Document.typeDefinition(a.typeCondition.name)(schema) //maybe as second parameter???

      val fields = for {
        selection <- a.selectionSet
        fieldName = GQLWriter[Selection].write(selection)(schema)
        field = gqlType
          .flatMap(_.children.find(_.name == fieldName))
          .map(GQLWriter[FieldDefinition].write(_)(schema))
          .getOrElse("Nothing")
      } yield s"$field"

      s"""case class ${a.name}(${fields.mkString(", ")})"""
    }
  }

  object QueryArgsWriter extends GQLWriter[QueryArgs] {
    override def write(queryDef: QueryArgs)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"case class ${queryDef.op.name}Args(${queryDef.op.args.map(arg => s"${arg._1}: ${GQLWriter[Type].write(arg._2)(schema)}").mkString(", ")})"
    }
  }

  object QueryDefWriter extends GQLWriter[QueryDef] {
    override def write(queryDef: QueryDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"${queryDef.op.name}: ${queryDef.op.name}Args => ${GQLWriter[Type].write(queryDef.op.ofType)(schema)}"
    }
  }

  object RootQueryDefWriter extends GQLWriter[RootQueryDef] {
    override def write(queryDef: RootQueryDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
         |${queryDef.op.children.map(c => GQLWriter[QueryArgs].write(QueryArgs(c))(schema)).mkString(",\n")}
         |case class Queries(
         |${queryDef.op.children.map(c => GQLWriter[QueryDef].write(QueryDef(c))(schema)).mkString(",\n")}
         |)""".stripMargin
    }
  }

  object MutationArgsWriter extends GQLWriter[MutationArgs] {
    override def write(mutationDef: MutationArgs)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"case class ${mutationDef.op.name}Args(${mutationDef.op.args
        .map(arg => s"${arg._1}: ${GQLWriter[Type].write(arg._2)(schema)}")
        .mkString(", ")})"
    }
  }

  object MutationDefWriter extends GQLWriter[MutationDef] {
    override def write(mutationDef: MutationDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"${mutationDef.op.name}: ${mutationDef.op.name}Args => ${GQLWriter[Type].write(mutationDef.op.ofType)(schema)}"
    }
  }

  object RootMutationDefWriter extends GQLWriter[RootMutationDef] {
    override def write(mutationDef: RootMutationDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
         |${mutationDef.op.children.map(c => GQLWriter[QueryArgs].write(QueryArgs(c))(schema)).mkString(",\n")}
         |case class Mutations(
         |${mutationDef.op.children.map(c => GQLWriter[QueryDef].write(QueryDef(c))(schema)).mkString(",\n")}
         |)""".stripMargin
    }
  }

  object SubscriptionArgsWriter extends GQLWriter[SubscriptionArgs] {
    override def write(
      subscriptionDef: SubscriptionArgs
    )(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"case class ${subscriptionDef.op.name}Args(${subscriptionDef.op.args
        .map(arg => s"${arg._1}: ${GQLWriter[Type].write(arg._2)(schema)}")
        .mkString(", ")})"
    }
  }

  object SubscriptionDefWriter extends GQLWriter[SubscriptionDef] {
    override def write(
      subscriptionDef: SubscriptionDef
    )(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"${subscriptionDef.op.name}: ${subscriptionDef.op.name}Args => ZStream[Console, Nothing, ${GQLWriter[Type].write(subscriptionDef.op.ofType)(schema)}]"
    }
  }

  object RootSubscriptionDefWriter extends GQLWriter[RootSubscriptionDef] {
    override def write(
      subscriptionDef: RootSubscriptionDef
    )(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
         |${subscriptionDef.op.children
           .map(c => GQLWriter[SubscriptionArgs].write(SubscriptionArgs(c))(schema))
           .mkString(",\n")}
         |case class Subscriptions(
         |${subscriptionDef.op.children
           .map(c => GQLWriter[SubscriptionDef].write(SubscriptionDef(c))(schema))
           .mkString(",\n")}
         |)""".stripMargin
    }
  }

  object TypeDefinitionWriter extends GQLWriter[TypeDefinition] {
    override def write(typedef: TypeDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""case class ${typedef.name}(${typedef.children
        .map(GQLWriter[FieldDefinition].write(_)(schema))
        .mkString(", ")})"""
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
