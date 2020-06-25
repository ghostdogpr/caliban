package caliban.tools

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

object SchemaWriter {

  def write(
    schema: Document,
    objectName: String = "",
    packageName: Option[String] = None,
    effect: String = "zio.UIO",
    typeMappings: Map[String, String] = Map.empty
  ): String = {
    val schemaDef = schema.schemaDefinition

    val argsTypes = schema.objectTypeDefinitions
      .flatMap(_.fields.filter(_.args.nonEmpty).map(writeArguments(_, typeMappings)))
      .mkString("\n")

    val unionTypes = schema.unionTypeDefinitions
      .map(union => (union, union.memberTypes.flatMap(schema.objectTypeDefinition)))
      .toMap

    val unions = unionTypes.map { case (union, objects) => writeUnion(union, objects, typeMappings) }.mkString("\n")

    val objects = schema.objectTypeDefinitions
      .filterNot(obj =>
        reservedType(obj) ||
          schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
          schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
          schemaDef.exists(_.subscription.getOrElse("Subscription") == obj.name) ||
          unionTypes.values.flatten.exists(_.name == obj.name)
      )
      .map(writeObject(_, typeMappings))
      .mkString("\n")

    val inputs = schema.inputObjectTypeDefinitions.map(writeInputObject(_, typeMappings)).mkString("\n")

    val enums = schema.enumTypeDefinitions.map(writeEnum).mkString("\n")

    val queries = schema
      .objectTypeDefinition(schemaDef.flatMap(_.query).getOrElse("Query"))
      .map(t => writeRootQueryOrMutationDef(t, effect, typeMappings))
      .getOrElse("")

    val mutations = schema
      .objectTypeDefinition(schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
      .map(t => writeRootQueryOrMutationDef(t, effect, typeMappings))
      .getOrElse("")

    val subscriptions = schema
      .objectTypeDefinition(schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
      .map(t => writeRootSubscriptionDef(t, typeMappings))
      .getOrElse("")

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
          ${if (hasSubscriptions) "import zio.stream.ZStream" else ""}

      $typesAndOperations
      """
  }

  def safeName(name: String): String = ClientWriter.safeName(name)

  def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
    typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

  def writeRootField(
    field: FieldDefinition,
    effect: String,
    typeMappings: Map[String, String]
  ): String = {
    val argsName = if (field.args.nonEmpty) s" ${field.name.capitalize}Args =>" else ""
    s"${safeName(field.name)}:$argsName $effect[${writeType(field.ofType, typeMappings)}]"
  }

  def writeRootQueryOrMutationDef(
    op: ObjectTypeDefinition,
    effect: String,
    typeMappings: Map[String, String]
  ): String =
    s"""
       |${writeDescription(op.description)}case class ${op.name}(
       |${op.fields.map(c => writeRootField(c, effect, typeMappings)).mkString(",\n")}
       |)""".stripMargin

  def writeSubscriptionField(
    field: FieldDefinition,
    typeMappings: Map[String, String]
  ): String =
    "%s:%s ZStream[Any, Nothing, %s]".format(
      safeName(field.name),
      if (field.args.nonEmpty) s" ${field.name.capitalize}Args =>" else "",
      writeType(field.ofType, typeMappings)
    )

  def writeRootSubscriptionDef(
    op: ObjectTypeDefinition,
    typeMappings: Map[String, String]
  ): String =
    s"""
       |${writeDescription(op.description)}case class ${op.name}(
       |${op.fields.map(c => writeSubscriptionField(c, typeMappings)).mkString(",\n")}
       |)""".stripMargin

  def writeObject(
    typedef: ObjectTypeDefinition,
    typeMappings: Map[String, String]
  ): String =
    s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
      .map(writeField(_, typedef, typeMappings))
      .mkString(", ")})"""

  def writeInputObject(
    typedef: InputObjectTypeDefinition,
    typeMappings: Map[String, String]
  ): String =
    s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
      .map(writeInputValue(_, typeMappings))
      .mkString(", ")})"""

  def writeEnum(typedef: EnumTypeDefinition): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends scala.Product with scala.Serializable

          object ${typedef.name} {
            ${typedef.enumValuesDefinition
      .map(v => s"${writeDescription(v.description)}case object ${safeName(v.enumValue)} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeUnion(
    typedef: UnionTypeDefinition,
    objects: List[ObjectTypeDefinition],
    typeMappings: Map[String, String]
  ): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends scala.Product with scala.Serializable

          object ${typedef.name} {
            ${objects
      .map(o => s"${writeObject(o, typeMappings)} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeField(
    field: FieldDefinition,
    of: ObjectTypeDefinition,
    typeMappings: Map[String, String]
  ): String =
    if (field.args.nonEmpty) {
      s"${writeDescription(field.description)}${safeName(field.name)}: ${of.name.capitalize}${field.name.capitalize}Args => ${writeType(field.ofType, typeMappings)}"
    } else {
      s"""${writeDescription(field.description)}${safeName(field.name)}: ${writeType(field.ofType, typeMappings)}"""
    }

  def writeInputValue(
    value: InputValueDefinition,
    typeMappings: Map[String, String]
  ): String =
    s"""${writeDescription(value.description)}${safeName(value.name)}: ${writeType(value.ofType, typeMappings)}"""

  def writeArguments(
    field: FieldDefinition,
    typeMappings: Map[String, String]
  ): String = {
    def fields(args: List[InputValueDefinition]): String =
      s"${args.map(arg => s"${safeName(arg.name)}: ${writeType(arg.ofType, typeMappings)}").mkString(", ")}"

    if (field.args.nonEmpty) {
      s"case class ${field.name.capitalize}Args(${fields(field.args)})"
    } else {
      ""
    }
  }

  def writeDescription(description: Option[String]): String =
    description.fold("") {
      case d if d.contains("\n") =>
        s"""@GQLDescription(\"\"\"$d\"\"\")
           |""".stripMargin
      case d =>
        s"""@GQLDescription("$d")
           |""".stripMargin
    }

  def writeType(
    t: Type,
    typeMappings: Map[String, String]
  ): String =
    t match {
      case NamedType(name, true)   => typeMappings.getOrElse(name, name)
      case NamedType(name, false)  => s"Option[${typeMappings.getOrElse(name, name)}]"
      case ListType(ofType, true)  => s"List[${writeType(ofType, typeMappings)}]"
      case ListType(ofType, false) => s"Option[List[${writeType(ofType, typeMappings)}]]"
    }

}
