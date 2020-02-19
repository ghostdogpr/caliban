package caliban.codegen

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

object SchemaWriter {

  def write(schema: Document, objectName: String = "", packageName: Option[String] = None): String = {
    val schemaDef = Document.schemaDefinitions(schema).headOption

    val argsTypes = Document
      .objectTypeDefinitions(schema)
      .flatMap(_.fields.filter(_.args.nonEmpty).map(writeArguments))
      .mkString("\n")

    val unionTypes = Document
      .unionTypeDefinitions(schema)
      .map(union => (union, union.memberTypes.flatMap(Document.objectTypeDefinition(schema, _))))
      .toMap

    val unions = unionTypes.map { case (union, objects) => writeUnion(union, objects) }.mkString("\n")

    val objects = Document
      .objectTypeDefinitions(schema)
      .filterNot(
        obj =>
          reservedType(obj) ||
            schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
            schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
            schemaDef.exists(_.subscription.getOrElse("Subscription") == obj.name) ||
            unionTypes.values.flatten.exists(_.name == obj.name)
      )
      .map(writeObject)
      .mkString("\n")

    val inputs = Document.inputObjectTypeDefinitions(schema).map(writeInputObject).mkString("\n")

    val enums = Document.enumTypeDefinitions(schema).map(writeEnum).mkString("\n")

    val queries = Document
      .objectTypeDefinition(schema, schemaDef.flatMap(_.query).getOrElse("Query"))
      .map(t => writeRootQueryOrMutationDef(t))
      .getOrElse("")

    val mutations = Document
      .objectTypeDefinition(schema, schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
      .map(t => writeRootQueryOrMutationDef(t))
      .getOrElse("")

    val subscriptions = Document
      .objectTypeDefinition(schema, schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
      .map(t => writeRootSubscriptionDef(t))
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

  def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
    typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

  def writeRootField(field: FieldDefinition): String = {
    val argsName = if (field.args.nonEmpty) s"${field.name.capitalize}Args" else "()"
    s"${field.name}: $argsName => ${writeType(field.ofType)}"
  }

  def writeRootQueryOrMutationDef(op: ObjectTypeDefinition): String =
    s"""
       |${writeDescription(op.description)}case class ${op.name}(
       |${op.fields.map(c => writeRootField(c)).mkString(",\n")}
       |)""".stripMargin

  def writeSubscriptionField(field: FieldDefinition): String =
    "%s: %s => ZStream[Any, Nothing, %s]".format(
      field.name,
      if (field.args.nonEmpty) s"${field.name.capitalize}Args" else "()",
      writeType(field.ofType)
    )

  def writeRootSubscriptionDef(op: ObjectTypeDefinition): String =
    s"""
       |${writeDescription(op.description)}case class ${op.name}(
       |${op.fields.map(c => writeSubscriptionField(c)).mkString(",\n")}
       |)""".stripMargin

  def writeObject(typedef: ObjectTypeDefinition): String =
    s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
      .map(writeField(_, typedef))
      .mkString(", ")})"""

  def writeInputObject(typedef: InputObjectTypeDefinition): String =
    s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
      .map(writeInputValue(_, typedef))
      .mkString(", ")})"""

  def writeEnum(typedef: EnumTypeDefinition): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends scala.Product with scala.Serializable

          object ${typedef.name} {
            ${typedef.enumValuesDefinition
      .map(v => s"${writeDescription(v.description)}case object ${v.enumValue} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeUnion(typedef: UnionTypeDefinition, objects: List[ObjectTypeDefinition]): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends scala.Product with scala.Serializable

          object ${typedef.name} {
            ${objects
      .map(o => s"${writeObject(o)} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeField(field: FieldDefinition, of: ObjectTypeDefinition): String =
    if (field.args.nonEmpty) {
      s"${writeDescription(field.description)}${field.name}: ${of.name.capitalize}${field.name.capitalize}Args => ${writeType(field.ofType)}"
    } else {
      s"""${writeDescription(field.description)}${field.name}: ${writeType(field.ofType)}"""
    }

  def writeInputValue(value: InputValueDefinition, of: InputObjectTypeDefinition): String =
    s"""${writeDescription(value.description)}${value.name}: ${writeType(value.ofType)}"""

  def writeArguments(field: FieldDefinition): String = {
    def fields(args: List[InputValueDefinition]): String =
      s"${args.map(arg => s"${arg.name}: ${writeType(arg.ofType)}").mkString(", ")}"

    if (field.args.nonEmpty) {
      s"case class ${field.name.capitalize}Args(${fields(field.args)})"
    } else {
      ""
    }
  }

  def writeDescription(description: Option[String]): String =
    description.fold("")(d => s"""@GQLDescription("$d")
                                 |""".stripMargin)

  def writeType(t: Type): String =
    t match {
      case NamedType(name, true)   => name
      case NamedType(name, false)  => s"Option[$name]"
      case ListType(ofType, true)  => s"List[${writeType(ofType)}]"
      case ListType(ofType, false) => s"Option[List[${writeType(ofType)}]]"
    }

}
