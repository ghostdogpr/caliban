package caliban.codegen

import caliban.codegen.Generator._
import caliban.parsing.adt.ExecutableDefinition.TypeDefinition
import caliban.parsing.adt.Type.{ FieldDefinition, ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

object ScalaWriter {
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

  object DocumentWriter extends GQLWriter[Document, Any] {
    override def write(schema: Document)(nothing: Any)(implicit context: GQLWriterContext): String = {
      import context._

      val argsTypes = Document
        .typeDefinitions(schema)
        .filterNot(reservedType)
        .flatMap(_.fields.filter(_.args.nonEmpty).map(c => GQLWriter[Args, String].write(Args(c))("")))
        .mkString("\n")

      val types = Document
        .typeDefinitions(schema)
        .filterNot(reservedType)
        .map(GQLWriter[TypeDefinition, Document].write(_)(schema))
        .mkString("\n")

      val queries = Document
        .typeDefinition(schema, "Query")
        .map(t => GQLWriter[RootQueryDef, Document].write(RootQueryDef(t))(schema))
        .getOrElse("")

      val mutations = Document
        .typeDefinition(schema, "Mutation")
        .map(t => GQLWriter[RootMutationDef, Document].write(RootMutationDef(t))(schema))
        .getOrElse("")

      val subscriptions = Document
        .typeDefinition(schema, "Subscription")
        .map(t => GQLWriter[RootSubscriptionDef, Document].write(RootSubscriptionDef(t))(schema))
        .getOrElse("")

      val hasSubscriptions = Document.typeDefinition(schema, "Subscription").nonEmpty
      val hasTypes         = argsTypes.length + types.length > 0
      val hasOperations    = queries.length + mutations.length + subscriptions.length > 0

      s"""${if (hasTypes) "import Types._\n" else ""}
      ${if (hasSubscriptions) "import zio.stream.ZStream" else ""}

      ${if (hasTypes)
        "object Types {\n" +
          argsTypes + "\n" +
          types + "\n" +
          "\n}\n"
      else ""}

      ${if (hasOperations)
        "object Operations {\n" +
          queries + "\n\n" +
          mutations + "\n\n" +
          subscriptions + "\n" +
          "\n}"
      else ""}
      """
    }

  }

  object QueryDefWriter extends GQLWriter[QueryDef, Document] {
    override def write(queryDef: QueryDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      val argsName = if (queryDef.op.args.nonEmpty) s"${queryDef.op.name.capitalize}Args" else "()"

      s"${queryDef.op.name}: $argsName => ${GQLWriter[Type, Any].write(queryDef.op.ofType)(Nil)}"
    }
  }

  object RootQueryDefWriter extends GQLWriter[RootQueryDef, Document] {
    override def write(queryDef: RootQueryDef)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""
         |${queryDef.op.fields
           .filter(_.args.nonEmpty)
           .map(c => GQLWriter[Args, String].write(Args(c))(""))
           .mkString(",\n")}
         |case class Query(
         |${queryDef.op.fields.map(c => GQLWriter[QueryDef, Document].write(QueryDef(c))(schema)).mkString(",\n")}
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
         |${mutationDef.op.fields
           .filter(_.args.nonEmpty)
           .map(c => GQLWriter[Args, String].write(Args(c))(""))
           .mkString(",\n")}
         |case class Mutation(
         |${mutationDef.op.fields.map(c => GQLWriter[QueryDef, Document].write(QueryDef(c))(schema)).mkString(",\n")}
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
         |${subscriptionDef.op.fields
           .filter(_.args.nonEmpty)
           .map(c => GQLWriter[Args, String].write(Args(c))(""))
           .mkString(",\n")}
         |case class Subscription(
         |${subscriptionDef.op.fields
           .map(c => GQLWriter[SubscriptionDef, Document].write(SubscriptionDef(c))(schema))
           .mkString(",\n")}
         |)""".stripMargin
    }
  }

  object TypeDefinitionWriter extends GQLWriter[TypeDefinition, Document] {
    override def write(typedef: TypeDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""case class ${typedef.name}(${typedef.fields
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
        case NamedType(name, true)   => convertType(name)
        case NamedType(name, false)  => s"Option[${convertType(name)}]"
        case ListType(ofType, true)  => s"List[${TypeWriter.write(ofType)(nothing)}]"
        case ListType(ofType, false) => s"Option[List[${TypeWriter.write(ofType)(nothing)}]]"
      }
  }

}
