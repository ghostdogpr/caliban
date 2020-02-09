package caliban.codegen

import caliban.codegen.Generator._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Type.{ FieldDefinition, ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

object ScalaWriter {
  val scalafmtConfig: String = """
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

  def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
    typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

  trait ScalaGQLWriter extends GQLWriterContext {
    override implicit val fieldWriter            = FieldWriter
    override implicit val typeWriter             = TypeWriter
    override implicit val objectWriter           = ObjectWriter
    override implicit val enumWriter             = EnumWriter
    override implicit val unionWriter            = UnionWriter
    override implicit val docWriter              = DocumentWriter
    override implicit val rootQueryWriter        = RootQueryDefWriter
    override implicit val queryWriter            = QueryDefWriter
    override implicit val rootMutationWriter     = RootMutationDefWriter
    override implicit val mutationWriter         = MutationDefWriter
    override implicit val rootSubscriptionWriter = RootSubscriptionDefWriter
    override implicit val subscriptionWriter     = SubscriptionDefWriter
    override implicit val argsWriter             = ArgsWriter
  }

  object DefaultGQLWriter extends ScalaGQLWriter

  object DocumentWriter extends GQLWriter[Document, Any] {
    override def write(schema: Document)(nothing: Any)(implicit context: GQLWriterContext): String = {
      import context._

      val argsTypes = Document
        .objectTypeDefinitions(schema)
        .filterNot(reservedType)
        .flatMap(_.fields.filter(_.args.nonEmpty).map(c => GQLWriter[Args, String].write(Args(c))("")))
        .mkString("\n")

      val unionTypes = Document
        .unionTypeDefinitions(schema)
        .map(union => Union(union, union.memberTypes.flatMap(Document.objectTypeDefinition(schema, _))))

      val unions = unionTypes
        .map(GQLWriter[Union, Document].write(_)(schema))
        .mkString("\n")

      val objects = Document
        .objectTypeDefinitions(schema)
        .filterNot(reservedType)
        .filterNot(obj => unionTypes.exists(_.objects.exists(_.name == obj.name)))
        .map(GQLWriter[ObjectTypeDefinition, Document].write(_)(schema))
        .mkString("\n")

      val enums = Document
        .enumTypeDefinitions(schema)
        .map(GQLWriter[EnumTypeDefinition, Document].write(_)(schema))
        .mkString("\n")

      val queries = Document
        .objectTypeDefinition(schema, "Query")
        .map(t => GQLWriter[RootQueryDef, Document].write(RootQueryDef(t))(schema))
        .getOrElse("")

      val mutations = Document
        .objectTypeDefinition(schema, "Mutation")
        .map(t => GQLWriter[RootMutationDef, Document].write(RootMutationDef(t))(schema))
        .getOrElse("")

      val subscriptions = Document
        .objectTypeDefinition(schema, "Subscription")
        .map(t => GQLWriter[RootSubscriptionDef, Document].write(RootSubscriptionDef(t))(schema))
        .getOrElse("")

      val hasSubscriptions = Document.objectTypeDefinition(schema, "Subscription").nonEmpty
      val hasTypes         = argsTypes.length + objects.length + enums.length + unions.length > 0
      val hasOperations    = queries.length + mutations.length + subscriptions.length > 0

      val typesAndOperations = s"""
      ${if (hasTypes)
        "object Types {\n" +
          argsTypes + "\n" +
          objects + "\n" +
          unions + "\n" +
          enums + "\n" +
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

      s"""${if (hasTypes && hasOperations) "import Types._\n" else ""}
          ${if (typesAndOperations.contains("@GQL")) "import caliban.schema.Annotations._\n" else ""}
          ${if (hasSubscriptions) "import zio.stream.ZStream" else ""}

      $typesAndOperations
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
         |${writeDescription(queryDef.op.description)}case class Query(
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
         |${writeDescription(mutationDef.op.description)}case class Mutation(
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
         |${writeDescription(subscriptionDef.op.description)}case class Subscription(
         |${subscriptionDef.op.fields
           .map(c => GQLWriter[SubscriptionDef, Document].write(SubscriptionDef(c))(schema))
           .mkString(",\n")}
         |)""".stripMargin
    }
  }

  object ObjectWriter extends GQLWriter[ObjectTypeDefinition, Document] {
    override def write(typedef: ObjectTypeDefinition)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
        .map(GQLWriter[FieldDefinition, ObjectTypeDefinition].write(_)(typedef))
        .mkString(", ")})"""
    }
  }

  object EnumWriter extends GQLWriter[EnumTypeDefinition, Document] {
    override def write(typedef: EnumTypeDefinition)(schema: Document)(implicit context: GQLWriterContext): String =
      s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends Product with Serializable

          object ${typedef.name} {
            ${typedef.enumValuesDefinition
        .map(v => s"${writeDescription(v.description)}case object ${v.enumValue} extends ${typedef.name}")
        .mkString("\n")}
          }
       """
  }

  object UnionWriter extends GQLWriter[Union, Document] {
    override def write(typedef: Union)(schema: Document)(implicit context: GQLWriterContext): String = {
      import context._

      s"""${writeDescription(typedef.typedef.description)}sealed trait ${typedef.typedef.name} extends Product with Serializable

          object ${typedef.typedef.name} {
            ${typedef.objects
        .map(o => s"${GQLWriter[ObjectTypeDefinition, Document].write(o)(schema)} extends ${typedef.typedef.name}")
        .mkString("\n")}
          }
       """
    }
  }

  object FieldWriter extends GQLWriter[FieldDefinition, ObjectTypeDefinition] {
    override def write(field: FieldDefinition)(of: ObjectTypeDefinition)(implicit context: GQLWriterContext): String = {
      import context._

      if (field.args.nonEmpty) {
        //case when field is parametrized
        s"${writeDescription(field.description)}${field.name}: ${of.name.capitalize}${field.name.capitalize}Args => ${GQLWriter[Type, Any]
          .write(field.ofType)(Nil)}"
      } else {
        s"""${writeDescription(field.description)}${field.name}: ${GQLWriter[Type, Any].write(field.ofType)(field)}"""
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

  def writeDescription(description: Option[String]): String =
    description.fold("")(d => s"""@GQLDescription("$d")
                                 |""".stripMargin)

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
