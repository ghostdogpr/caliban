package caliban.codegen

import caliban.parsing.adt.{ Document, ExecutableDefinition, Selection, Type }
import caliban.parsing.adt.Type.{ FieldDefinition, ListType, NamedType }
import Generator._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition, TypeDefinition }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }

object ScalaWriter {
  trait ScalaGQLWriter extends GQLWriterContext {
    implicit val fieldWriter            = FieldWriter
    implicit val typeWriter             = TypeWriter
    implicit val typeDefWriter          = TypeDefinitionWriter
    implicit val docWriter              = DocumentWriter
    implicit val rootQueryWriter        = RootQueryDefWriter
    implicit val queryWriter            = QueryDefWriter
    implicit val rootMutationWriter     = RootMutationDefWriter
    implicit val mutationWriter         = MutationDefWriter
    implicit val rootSubscriptionWriter = RootSubscriptionDefWriter
    implicit val subscriptionWriter     = SubscriptionDefWriter
    implicit val argsWriter             = ArgsWriter
  }

  object DefaultGQLWriter extends ScalaGQLWriter

  val scalafmtConfig = """
                         |version = "2.2.1"
                         |
                         |maxColumn = 120
                         |align = most
                         |continuationIndent.defnSite = 2
                         |assumeStandardLibraryStripMargin = true
                         |docstrings = JavaDoc
                         |lineEndings = preserve
                         |includeCurlyBraceInSelectChains = false
                         |danglingParentheses = true
                         |spaces {
                         |  inImportCurlyBraces = true
                         |}
                         |optIn.annotationNewlines = true
                         |
                         |rewrite.rules = [SortImports, RedundantBraces]
                         |""".stripMargin

  def reservedType(typeDefinition: TypeDefinition): Boolean =
    typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

  object DocumentWriter extends GQLWriter[Document, Any] {
    override def write(schema: Document)(nothing: Any)(implicit context: GQLWriterContext): String = {
      import context._

      val hasSubscriptions = Document.typeDefinition(schema, "Subscription").nonEmpty

      s"""import Types._
      import Fragments._
      ${if (hasSubscriptions) "import zio.stream.ZStream" else ""}

      object Types {
        ${Document
        .typeDefinitions(schema)
        .filterNot(reservedType)
        .flatMap(_.children.filter(_.args.nonEmpty).map(c => GQLWriter[Args, String].write(Args(c))("")))
        .mkString("\n")}
        ${Document
        .typeDefinitions(schema)
        .filterNot(reservedType)
        .map(GQLWriter[TypeDefinition, Document].write(_)(schema))
        .mkString("\n")}
      }

      object Operations {
        ${Document
        .typeDefinition(schema, "Query")
        .map(t => GQLWriter[RootQueryDef, Document].write(RootQueryDef(t))(schema))
        .getOrElse("")}

        ${Document
        .typeDefinition(schema, "Mutation")
        .map(t => GQLWriter[RootMutationDef, Document].write(RootMutationDef(t))(schema))
        .getOrElse("")}

        ${Document
        .typeDefinition(schema, "Subscription")
        .map(t => GQLWriter[RootSubscriptionDef, Document].write(RootSubscriptionDef(t))(schema))
        .getOrElse("")}
      }"""
    }

  }

  object QueryDefWriter extends GQLWriter[QueryDef, Document] {
    override def write(queryDef: QueryDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      val argsName = if (queryDef.op.args.nonEmpty) s"${queryDef.op.name.capitalize}Args" else "()"

      s"${queryDef.op.name}: ${argsName} => ${GQLWriter[Type, Any].write(queryDef.op.ofType)(Nil)}"
    }
  }

  object RootQueryDefWriter extends GQLWriter[RootQueryDef, Document] {
    override def write(queryDef: RootQueryDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
         |${queryDef.op.children
           .filter(_.args.nonEmpty)
           .map(c => GQLWriter[Args, String].write(Args(c))(""))
           .mkString(",\n")}
         |case class Queries(
         |${queryDef.op.children.map(c => GQLWriter[QueryDef, Document].write(QueryDef(c))(schema)).mkString(",\n")}
         |)""".stripMargin
    }
  }

  object MutationDefWriter extends GQLWriter[MutationDef, Document] {
    override def write(mutationDef: MutationDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      val argsName = if (mutationDef.op.args.nonEmpty) s"${mutationDef.op.name.capitalize}Args" else "()"

      s"${mutationDef.op.name}: $argsName => ${GQLWriter[Type, Any].write(mutationDef.op.ofType)(Nil)}"
    }
  }

  object RootMutationDefWriter extends GQLWriter[RootMutationDef, Document] {
    override def write(mutationDef: RootMutationDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
         |${mutationDef.op.children
           .filter(_.args.nonEmpty)
           .map(c => GQLWriter[Args, String].write(Args(c))(""))
           .mkString(",\n")}
         |case class Mutations(
         |${mutationDef.op.children.map(c => GQLWriter[QueryDef, Document].write(QueryDef(c))(schema)).mkString(",\n")}
         |)""".stripMargin
    }
  }

  object SubscriptionDefWriter extends GQLWriter[SubscriptionDef, Document] {
    override def write(
      subscriptionDef: SubscriptionDef
    )(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      "%s: %s => ZStream[Any, Nothing, %s]".format(
        subscriptionDef.op.name,
        if (subscriptionDef.op.args.nonEmpty) s"${subscriptionDef.op.name.capitalize}Args" else "()",
        GQLWriter[Type, Any]
          .write(subscriptionDef.op.ofType)(Nil)
      )
    }
  }

  object RootSubscriptionDefWriter extends GQLWriter[RootSubscriptionDef, Document] {
    override def write(
      subscriptionDef: RootSubscriptionDef
    )(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
         |${subscriptionDef.op.children
           .filter(_.args.nonEmpty)
           .map(c => GQLWriter[Args, String].write(Args(c))(""))
           .mkString(",\n")}
         |case class Subscriptions(
         |${subscriptionDef.op.children
           .map(c => GQLWriter[SubscriptionDef, Document].write(SubscriptionDef(c))(schema))
           .mkString(",\n")}
         |)""".stripMargin
    }
  }

  object TypeDefinitionWriter extends GQLWriter[TypeDefinition, Document] {
    override def write(typedef: TypeDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""case class ${typedef.name}(${typedef.children
        .map(GQLWriter[FieldDefinition, TypeDefinition].write(_)(typedef))
        .mkString(", ")})"""
    }
  }

  object FieldWriter extends GQLWriter[FieldDefinition, TypeDefinition] {
    override def write(field: FieldDefinition)(of: TypeDefinition)(implicit context: GQLWriterContext): String = {
      import context._

      if (field.args.nonEmpty) {
        //case when field is parametrized
        s"${field.name}: ${of.name.capitalize}${field.name.capitalize}Args => ${GQLWriter[Type, Any].write(field.ofType)(Nil)}"
      } else {
        s"""${field.name}: ${GQLWriter[Type, Any].write(field.ofType)(field)}"""
      }
    }
  }

  object ArgsWriter extends GQLWriter[Args, String] {
    override def write(arg: Args)(prefix: String)(implicit context: GQLWriterContext): String =
      if (arg.field.args.nonEmpty) {
        //case when field is parametrized
        s"case class ${prefix.capitalize}${arg.field.name.capitalize}Args(${fields(arg.field.args)})"
      } else {
        ""
      }

    def fields(args: List[(String, Type)])(implicit context: GQLWriterContext): String = {
      import context._
      s"${args.map(arg => s"${arg._1}: ${GQLWriter[Type, Any].write(arg._2)(Nil)}").mkString(", ")}"
    }
  }

  object TypeWriter extends GQLWriter[Type, Any] {
    def convertType(name: String): String = name match {
      case _ => name
    }

    override def write(t: Type)(nothing: Any)(implicit context: GQLWriterContext): String =
      t match {
        case NamedType(name, nonNull) if (nonNull)   => convertType(name)
        case NamedType(name, nonNull) if (!nonNull)  => s"Option[${convertType(name)}]"
        case ListType(ofType, nonNull) if (nonNull)  => s"List[${TypeWriter.write(ofType)(nothing)}]"
        case ListType(ofType, nonNull) if (!nonNull) => s"Option[List[${TypeWriter.write(ofType)(nothing)}]]"
      }
  }

}
