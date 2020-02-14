package caliban.codegen

import scala.annotation.tailrec
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

object ClientWriter {

  def write(schema: Document): String = {
//    val schemaDef = Document.schemaDefinitions(schema).headOption
//
//    val argsTypes = Document
//      .objectTypeDefinitions(schema)
//      .flatMap(_.fields.filter(_.args.nonEmpty).map(c => writeArguments(c)("")))
//      .mkString("\n")
//
//    val unionTypes = Document
//      .unionTypeDefinitions(schema)
//      .map(union => (union, union.memberTypes.flatMap(Document.objectTypeDefinition(schema, _))))
//      .toMap
//
//    val unions = unionTypes.map { case (union, objects) => writeUnion(union, objects) }.mkString("\n")

    val typesMap: Map[String, TypeDefinition] = schema.definitions.collect {
      case op @ ObjectTypeDefinition(_, name, _, _)      => name -> op
      case op @ InputObjectTypeDefinition(_, name, _, _) => name -> op
      case op @ EnumTypeDefinition(_, name, _, _)        => name -> op
      case op @ UnionTypeDefinition(_, name, _, _)       => name -> op
      case op @ ScalarTypeDefinition(_, name, _)         => name -> op
    }.toMap

    val objects = Document
      .objectTypeDefinitions(schema)
      //      .filterNot(obj => unionTypes.values.flatten.exists(_.name == obj.name))
      .map(writeObject(_, typesMap))
      .mkString("\n")
//
//    val inputs = Document.inputObjectTypeDefinitions(schema).map(writeInputObject).mkString("\n")
//
//    val enums = Document.enumTypeDefinitions(schema).map(writeEnum).mkString("\n")
//
//    val queries = Document
//      .objectTypeDefinition(schema, schemaDef.flatMap(_.query).getOrElse("Query"))
//      .map(t => writeRootQueryOrMutationDef(t))
//      .getOrElse("")
//
//    val mutations = Document
//      .objectTypeDefinition(schema, schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
//      .map(t => writeRootQueryOrMutationDef(t))
//      .getOrElse("")
//
//    val subscriptions = Document
//      .objectTypeDefinition(schema, schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
//      .map(t => writeRootSubscriptionDef(t))
//      .getOrElse("")
//
//    val hasSubscriptions = subscriptions.nonEmpty
//    val hasTypes         = argsTypes.length + objects.length + enums.length + unions.length + inputs.length > 0
//    val hasOperations    = queries.length + mutations.length + subscriptions.length > 0
//
//    val typesAndOperations = s"""
//      ${if (hasTypes)
//      "object Types {\n" +
//        argsTypes + "\n" +
//        objects + "\n" +
//        inputs + "\n" +
//        unions + "\n" +
//        enums + "\n" +
//        "\n}\n"
//    else ""}
//
//      ${if (hasOperations)
//      "object Operations {\n" +
//        queries + "\n\n" +
//        mutations + "\n\n" +
//        subscriptions + "\n" +
//        "\n}"
//    else ""}
//      """
//
//    s"""${if (hasTypes && hasOperations) "import Types._\n" else ""}
//          ${if (typesAndOperations.contains("@GQL")) "import caliban.schema.Annotations._\n" else ""}
//          ${if (hasSubscriptions) "import zio.stream.ZStream" else ""}
//
//      $typesAndOperations
//      """

    s"""object Data {
       |
       |  $objects
       |  
       |}""".stripMargin
  }

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

  def writeObject(typedef: ObjectTypeDefinition, typesMap: Map[String, TypeDefinition]): String =
    s"""type ${typedef.name}
       |object ${typedef.name} {
       |  ${typedef.fields.map(writeField(_, typedef, typesMap)).mkString("\n  ")}
       |}
       |""".stripMargin

  def writeInputObject(typedef: InputObjectTypeDefinition): String =
    s"""${writeDescription(typedef.description)}case class ${typedef.name}(${typedef.fields
      .map(writeInputValue(_, typedef))
      .mkString(", ")})"""

  def writeEnum(typedef: EnumTypeDefinition): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends Product with Serializable

          object ${typedef.name} {
            ${typedef.enumValuesDefinition
      .map(v => s"${writeDescription(v.description)}case object ${v.enumValue} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeUnion(
    typedef: UnionTypeDefinition,
    objects: List[ObjectTypeDefinition],
    typesMap: Map[String, TypeDefinition]
  ): String =
    s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends Product with Serializable

          object ${typedef.name} {
            ${objects
      .map(o => s"${writeObject(o, typesMap)} extends ${typedef.name}")
      .mkString("\n")}
          }
       """

  def writeField(field: FieldDefinition, of: ObjectTypeDefinition, typesMap: Map[String, TypeDefinition]): String = {
    val name      = field.name
    val typeName  = of.name
    val fieldType = getTypeName(field.ofType)
    val isScalar = typesMap
      .get(fieldType)
      .collect {
        case _: ScalarTypeDefinition => true
        case _                       => false
      }
      .getOrElse(true)
    val (typeParam, innerSelection, outputType, builder) =
      if (isScalar) {
        (
          "",
          "",
          writeType(field.ofType),
          writeTypeBuilder(field.ofType, "Scalar()")
        )
      } else {
        (
          "[A]",
          s"(innerSelection: SelectionBuilder[$fieldType, A])",
          writeType(field.ofType).replace(fieldType, "A"),
          writeTypeBuilder(field.ofType, "Obj(innerSelection)")
        )
      }
    // TODO arguments

    s"""def $name$typeParam$innerSelection: SelectionBuilder[$typeName, $outputType] = Field("$name", $builder)"""

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

  def writeTypeBuilder(t: Type, inner: String): String =
    t match {
      case NamedType(_, true)  => inner
      case NamedType(_, false) => s"OptionOf($inner)"
      case ListType(_, true)   => s"ListOf($inner)"
      case ListType(_, false)  => s"OptionOf($inner)"
    }

  @tailrec
  def getTypeName(t: Type): String = t match {
    case NamedType(name, _)  => name
    case ListType(ofType, _) => getTypeName(ofType)
  }
}
