package caliban.tools

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }
import caliban.tools.implicits.Implicits._
import caliban.tools.implicits.ScalarMappings

object SchemaWriter {

  def write(
    schema: Document,
    packageName: Option[String] = None,
    effect: String = "zio.UIO",
    imports: Option[List[String]] = None
  )(implicit scalarMappings: ScalarMappings): String = {
    val schemaDef = schema.schemaDefinition

    val argsTypes = schema.objectTypeDefinitions
      .flatMap(typeDef => typeDef.fields.filter(_.args.nonEmpty).map(writeArguments(_, typeDef)))
      .mkString("\n")

    val unionTypes = schema.unionTypeDefinitions
      .map(union => (union, union.memberTypes.flatMap(schema.objectTypeDefinition)))
      .toMap

    val unions = unionTypes.map { case (union, objects) => writeUnion(union, objects) }.mkString("\n")

    val objects = schema.objectTypeDefinitions
      .filterNot(obj =>
        reservedType(obj) ||
          schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
          schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
          schemaDef.exists(_.subscription.getOrElse("Subscription") == obj.name) ||
          unionTypes.values.flatten.exists(_.name == obj.name)
      )
      .map(writeObject)
      .mkString("\n")

    val inputs = schema.inputObjectTypeDefinitions.map(writeInputObject).mkString("\n")

    val enums = schema.enumTypeDefinitions.map(writeEnum).mkString("\n")

    val queries = schema
      .objectTypeDefinition(schemaDef.flatMap(_.query).getOrElse("Query"))
      .map(t => writeRootQueryOrMutationDef(t, effect))
      .getOrElse("")

    val mutations = schema
      .objectTypeDefinition(schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
      .map(t => writeRootQueryOrMutationDef(t, effect))
      .getOrElse("")

    val subscriptions = schema
      .objectTypeDefinition(schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
      .map(t => writeRootSubscriptionDef(t))
      .getOrElse("")

    val additionalImportsString = imports.fold("")(_.map(i => s"import $i").mkString("\n"))

    val hasSubscriptions = subscriptions.nonEmpty
    val hasTypes         = argsTypes.length + objects.length + enums.length + unions.length + inputs.length > 0
    val hasOperations    = queries.length + mutations.length + subscriptions.length > 0

    val typesAndOperations = s"""
      ${if (hasTypes)
      "object Types {\n" +
        argsTypes + "\n" +
        objects + "\n" +
        inputs + "\n" +
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

    s"""${packageName.fold("")(p => s"package $p\n\n")}${if (hasTypes && hasOperations) "import Types._\n" else ""}
          ${if (typesAndOperations.contains("@GQL")) "import caliban.schema.Annotations._\n" else ""}
          ${if (hasSubscriptions) "import zio.stream.ZStream\n" else ""}
          $additionalImportsString

      $typesAndOperations
      """
  }

  def safeName(name: String): String = ClientWriter.safeName(name)

  def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
    typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

  def writeRootField(field: FieldDefinition, od: ObjectTypeDefinition, effect: String)(implicit
    scalarMappings: ScalarMappings
  ): String = {
    val argsTypeName = if (field.args.nonEmpty) s" ${argsName(field, od)} =>" else ""
    s"${safeName(field.name)} :$argsTypeName $effect[${writeType(field.ofType)}]"
  }

  def writeRootQueryOrMutationDef(op: ObjectTypeDefinition, effect: String)(implicit
    scalarMappings: ScalarMappings
  ): String =
    s"""
       |${writeDescription(op.description)}case class ${op.name}(
       |${op.fields.map(c => writeRootField(c, op, effect)).mkString(",\n")}
       |)""".stripMargin

  def writeSubscriptionField(field: FieldDefinition, od: ObjectTypeDefinition)(implicit
    scalarMappings: ScalarMappings
  ): String =
    "%s:%s ZStream[Any, Nothing, %s]".format(
      safeName(field.name),
      if (field.args.nonEmpty) s" ${argsName(field, od)} =>" else "",
      writeType(field.ofType)
    )

  def writeRootSubscriptionDef(op: ObjectTypeDefinition)(implicit scalarMappings: ScalarMappings): String =
    s"""
       |${writeDescription(op.description)}case class ${op.name}(
       |${op.fields.map(c => writeSubscriptionField(c, op)).mkString(",\n")}
       |)""".stripMargin

  def writeObject(typedef: ObjectTypeDefinition)(implicit scalarMappings: ScalarMappings): String =
    s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
      .map(writeField(_, typedef))
      .mkString(", ")})"""

  def writeInputObject(typedef: InputObjectTypeDefinition)(implicit scalarMappings: ScalarMappings): String =
    s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
      .map(writeInputValue)
      .mkString(", ")})"""

  def writeEnum(typedef: EnumTypeDefinition): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends scala.Product with scala.Serializable

          object ${typedef.name} {
            ${typedef.enumValuesDefinition
      .map(v => s"${writeDescription(v.description)}case object ${safeName(v.enumValue)} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeUnion(typedef: UnionTypeDefinition, objects: List[ObjectTypeDefinition])(implicit
    scalarMappings: ScalarMappings
  ): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends scala.Product with scala.Serializable

          object ${typedef.name} {
            ${objects
      .map(o => s"${writeObject(o)} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeField(field: FieldDefinition, of: ObjectTypeDefinition)(implicit scalarMappings: ScalarMappings): String =
    if (field.args.nonEmpty) {
      s"${writeDescription(field.description)}${safeName(field.name)} : ${argsName(field, of)} => ${writeType(field.ofType)}"
    } else {
      s"""${writeDescription(field.description)}${safeName(field.name)} : ${writeType(field.ofType)}"""
    }

  def writeInputValue(value: InputValueDefinition)(implicit scalarMappings: ScalarMappings): String =
    s"""${writeDescription(value.description)}${safeName(value.name)} : ${writeType(value.ofType)}"""

  def writeArguments(field: FieldDefinition, of: ObjectTypeDefinition)(implicit
    scalarMappings: ScalarMappings
  ): String = {
    def fields(args: List[InputValueDefinition]): String =
      s"${args.map(arg => s"${safeName(arg.name)} : ${writeType(arg.ofType)}").mkString(", ")}"

    if (field.args.nonEmpty) {
      s"case class ${argsName(field, of)}(${fields(field.args)})"
    } else {
      ""
    }
  }

  private def argsName(field: FieldDefinition, od: ObjectTypeDefinition): String =
    s"${od.name.capitalize}${field.name.capitalize}Args"

  def escapeDoubleQuotes(input: String): String =
    input.replace("\"", "\\\"")

  def writeDescription(description: Option[String]): String =
    description.fold("") {
      case d if d.contains("\n") =>
        s"""@GQLDescription(\"\"\"${escapeDoubleQuotes(d)}\"\"\")
           |""".stripMargin
      case d                     =>
        s"""@GQLDescription("${escapeDoubleQuotes(d)}")
           |""".stripMargin
    }

  def writeType(t: Type)(implicit scalarMappings: ScalarMappings): String = t match {
    case NamedType(name, true)   => scalarMappings.flatMap(m => m.get(name)).getOrElse(name)
    case NamedType(name, false)  => s"Option[${scalarMappings.flatMap(m => m.get(name)).getOrElse(name)}]"
    case ListType(ofType, true)  => s"List[${writeType(ofType)}]"
    case ListType(ofType, false) => s"Option[List[${writeType(ofType)}]]"
  }
}
