package caliban.tools

import scala.annotation.tailrec
import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

object ClientWriter {

  def write(
    schema: Document,
    objectName: String = "Client",
    packageName: Option[String] = None,
    effect: String = "zio.UIO"
  ): String =
    write(schema, objectName, packageName, genView = false)

  def write(
    schema: Document,
    objectName: String,
    packageName: Option[String],
    genView: Boolean
  ): String = {
    val schemaDef = schema.schemaDefinition

    val mappingClashedTypeNames = getMappingsClashedNames(
      schema.definitions.collect {
        case ObjectTypeDefinition(_, name, _, _, _)   => name
        case InputObjectTypeDefinition(_, name, _, _) => name
        case EnumTypeDefinition(_, name, _, _)        => name
        case UnionTypeDefinition(_, name, _, _)       => name
        case ScalarTypeDefinition(_, name, _)         => name
        case InterfaceTypeDefinition(_, name, _, _)   => name
      }
    )

    val typesMap: Map[String, TypeDefinition] = schema.definitions.collect {
      case op @ ObjectTypeDefinition(_, name, _, _, _)   => name -> op
      case op @ InputObjectTypeDefinition(_, name, _, _) => name -> op
      case op @ EnumTypeDefinition(_, name, _, _)        => name -> op
      case op @ UnionTypeDefinition(_, name, _, _)       => name -> op
      case op @ ScalarTypeDefinition(_, name, _)         => name -> op
      case op @ InterfaceTypeDefinition(_, name, _, _)   => name -> op
    }.map {
      case (name, op) => safeTypeName(name, mappingClashedTypeNames) -> op
    }.toMap

    val objects = schema.objectTypeDefinitions
      .filterNot(obj =>
        reservedType(obj) ||
          schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
          schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
          schemaDef.exists(_.subscription.getOrElse("Subscription") == obj.name)
      )
      .map(writeObject(_, typesMap, mappingClashedTypeNames, genView))
      .mkString("\n")

    val inputs = schema.inputObjectTypeDefinitions.map(writeInputObject(_, mappingClashedTypeNames)).mkString("\n")

    val enums = schema.enumTypeDefinitions.map(writeEnum(_, mappingClashedTypeNames)).mkString("\n")

    val scalars = schema.scalarTypeDefinitions
      .filterNot(s => supportedScalars.contains(s.name))
      .map(writeScalar(_, mappingClashedTypeNames))
      .mkString("\n")

    val queries = schema
      .objectTypeDefinition(schemaDef.flatMap(_.query).getOrElse("Query"))
      .map(t => writeRootQuery(t, typesMap, mappingClashedTypeNames))
      .getOrElse("")

    val mutations = schema
      .objectTypeDefinition(schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
      .map(t => writeRootMutation(t, typesMap, mappingClashedTypeNames))
      .getOrElse("")

    val subscriptions = schema
      .objectTypeDefinition(schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
      .map(t => writeRootSubscription(t, typesMap, mappingClashedTypeNames))
      .getOrElse("")

    val imports = s"""${if (enums.nonEmpty)
      """import caliban.client.CalibanClientError.DecodingError
        |""".stripMargin
    else ""}${if (objects.nonEmpty || queries.nonEmpty || mutations.nonEmpty || subscriptions.nonEmpty)
      """import caliban.client.FieldBuilder._
        |import caliban.client.SelectionBuilder._
        |""".stripMargin
    else
      ""}${if (enums.nonEmpty || objects.nonEmpty || queries.nonEmpty || mutations.nonEmpty || subscriptions.nonEmpty || inputs.nonEmpty)
      """import caliban.client._
        |""".stripMargin
    else ""}${if (queries.nonEmpty || mutations.nonEmpty || subscriptions.nonEmpty)
      """import caliban.client.Operations._
        |""".stripMargin
    else ""}${if (enums.nonEmpty || inputs.nonEmpty)
      """import caliban.client.__Value._
        |""".stripMargin
    else ""}"""

    s"""${packageName.fold("")(p => s"package $p\n\n")}$imports
       |
       |object $objectName {
       |
       |  $scalars
       |  $enums
       |  $objects
       |  $inputs
       |  $queries
       |  $mutations
       |  $subscriptions
       |  
       |}""".stripMargin
  }

  private def getMappingsClashedNames(typeNames: List[String]): Map[String, String] =
    typeNames
      .map(name => name.toLowerCase -> name)
      .groupBy(_._1)
      .collect {
        case (_, (_ :: typeNamesToRename)) if typeNamesToRename.nonEmpty =>
          typeNamesToRename.zipWithIndex.map {
            case (((_, originalTypeName), index)) =>
              val suffix = "_" * (index + 1)
              originalTypeName -> s"$originalTypeName$suffix"
          }.toMap
      }
      .reduceOption(_ ++ _)
      .getOrElse(Map.empty)

  private def safeTypeName(typeName: String, mappingClashedTypeNames: Map[String, String]): String =
    mappingClashedTypeNames.getOrElse(typeName, typeName)

  def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
    typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

  def writeRootQuery(
    typedef: ObjectTypeDefinition,
    typesMap: Map[String, TypeDefinition],
    mappingClashedTypeNames: Map[String, String]
  ): String =
    s"""type ${typedef.name} = RootQuery
       |object ${typedef.name} {
       |  ${typedef.fields.map(writeField(_, "RootQuery", typesMap, mappingClashedTypeNames)).mkString("\n  ")}
       |}
       |""".stripMargin

  def writeRootMutation(
    typedef: ObjectTypeDefinition,
    typesMap: Map[String, TypeDefinition],
    mappingClashedTypeNames: Map[String, String]
  ): String =
    s"""type ${typedef.name} = RootMutation
       |object ${typedef.name} {
       |  ${typedef.fields.map(writeField(_, "RootMutation", typesMap, mappingClashedTypeNames)).mkString("\n  ")}
       |}
       |""".stripMargin

  def writeRootSubscription(
    typedef: ObjectTypeDefinition,
    typesMap: Map[String, TypeDefinition],
    mappingClashedTypeNames: Map[String, String]
  ): String =
    s"""type ${typedef.name} = RootSubscription
       |object ${typedef.name} {
       |  ${typedef.fields.map(writeField(_, "RootSubscription", typesMap, mappingClashedTypeNames)).mkString("\n  ")}
       |}
       |""".stripMargin

  def writeObject(
    typedef: ObjectTypeDefinition,
    typesMap: Map[String, TypeDefinition],
    mappingClashedTypeNames: Map[String, String],
    genView: Boolean
  ): String = {
    val objectName: String = safeTypeName(typedef.name, mappingClashedTypeNames)
    val fields             = typedef.fields.map(collectFieldInfo(_, objectName, typesMap, mappingClashedTypeNames))
    val view               = if (genView) "\n  " + writeView(typedef.name, fields.map(_.typeInfo), mappingClashedTypeNames) else ""

    s"""type $objectName
       |object $objectName {$view
       |  ${fields.map(writeFieldInfo).mkString("\n  ")}
       |}
       |""".stripMargin
  }

  def writeView(
    objectName: String,
    fields: List[FieldTypeInfo],
    mappingClashedTypeNames: Map[String, String]
  ): String = {
    val viewName       = s"${objectName}View"
    val safeObjectName = safeTypeName(objectName, mappingClashedTypeNames)

    def argumentName(fieldName: String, argName: String): String =
      fieldName + argName.capitalize

    def withRoundBrackets(input: List[String]): String =
      if (input.nonEmpty) input.mkString("(", ", ", ")") else ""

    val genericSelectionFields =
      fields.collect {
        case field if field.owner.nonEmpty =>
          field -> s"${field.name}Selection"
      }

    val genericSelectionFieldTypes =
      genericSelectionFields.map { case (field, name) => (field, name.capitalize) }

    val genericSelectionFieldsMap     = genericSelectionFields.toMap
    val genericSelectionFieldTypesMap = genericSelectionFieldTypes.toMap

    val viewFunctionArguments: List[String] =
      fields.collect {
        case field if field.arguments.nonEmpty =>
          writeArgumentFields(
            field.arguments.map(a => a.copy(name = argumentName(field.name, a.name))),
            mappingClashedTypeNames
          )
      }

    val viewFunctionSelectionArguments: List[String] =
      genericSelectionFields.collect {
        case (field @ FieldTypeInfo(_, _, _, Some(owner)), fieldName) =>
          val tpe = genericSelectionFieldTypesMap(field)
          s"$fieldName: SelectionBuilder[$owner, $tpe]"
      }

    val viewClassFields: List[String] =
      fields.map {
        case field @ FieldTypeInfo(_, outputType, _, Some(_)) =>
          val tpeName = genericSelectionFieldTypesMap(field)
          val tpe =
            if (outputType.endsWith("[A]")) outputType.dropRight(3) + "[" + tpeName + "]"
            else outputType.dropRight(1) + tpeName
          s"${field.name}: $tpe"

        case field @ FieldTypeInfo(_, outputType, _, _) =>
          s"${field.name}: $outputType"
      }

    val viewFunctionBody: String =
      fields.map { field =>
        val argsPart      = withRoundBrackets(field.arguments.map(a => argumentName(field.name, a.name)))
        val selectionPart = withRoundBrackets(genericSelectionFieldsMap.get(field).toList)
        s"${field.name}$argsPart$selectionPart"
      }.mkString(" ~ ")

    val viewClassFieldParams: String = withRoundBrackets(viewClassFields)

    val viewFunction: String =
      fields match {
        case head :: Nil =>
          s"$viewFunctionBody.map(${head.name} => $viewName(${head.name}))"

        case other =>
          val unapply = fields.tail.foldLeft(fields.head.name) {
            case (acc, field) => "(" + acc + ", " + field.name + ")"
          }
          s"($viewFunctionBody).map { case $unapply => $viewName(${other.map(_.name).mkString(", ")}) }"
      }

    val typeParams =
      if (genericSelectionFieldTypes.nonEmpty) genericSelectionFieldTypes.map(_._2).mkString("[", ", ", "]") else ""

    val viewFunctionArgs          = withRoundBrackets(viewFunctionArguments)
    val viewFunctionSelectionArgs = withRoundBrackets(viewFunctionSelectionArguments)

    s"""
       |final case class $viewName$typeParams$viewClassFieldParams
       |
       |type ViewSelection$typeParams = SelectionBuilder[$safeObjectName, $viewName$typeParams]
       |
       |def view$typeParams$viewFunctionArgs$viewFunctionSelectionArgs: ViewSelection$typeParams = $viewFunction
       |""".stripMargin
  }

  def writeInputObject(typedef: InputObjectTypeDefinition, mappingClashedTypeNames: Map[String, String]): String = {
    val inputObjectName = safeTypeName(typedef.name, mappingClashedTypeNames)
    s"""case class $inputObjectName(${writeArgumentFields(typedef.fields, mappingClashedTypeNames)})
       |object $inputObjectName {
       |  implicit val encoder: ArgEncoder[$inputObjectName] = new ArgEncoder[$inputObjectName] {
       |    override def encode(value: $inputObjectName): __Value =
       |      __ObjectValue(List(${typedef.fields
         .map(f =>
           s""""${f.name}" -> ${writeInputValue(
             f.ofType,
             s"value.${safeName(f.name)}",
             inputObjectName,
             mappingClashedTypeNames
           )}"""
         )
         .mkString(", ")}))
       |    override def typeName: String = "$inputObjectName"
       |  }
       |}""".stripMargin
  }

  def writeInputValue(
    t: Type,
    fieldName: String,
    typeName: String,
    mappingClashedTypeNames: Map[String, String]
  ): String =
    t match {
      case NamedType(name, true) =>
        if (name == typeName) s"encode($fieldName)"
        else s"implicitly[ArgEncoder[${mapTypeName(name, mappingClashedTypeNames)}]].encode($fieldName)"
      case NamedType(name, false) =>
        s"$fieldName.fold(__NullValue: __Value)(value => ${writeInputValue(NamedType(name, nonNull = true), "value", typeName, mappingClashedTypeNames)})"
      case ListType(ofType, true) =>
        s"__ListValue($fieldName.map(value => ${writeInputValue(ofType, "value", typeName, mappingClashedTypeNames)}))"
      case ListType(ofType, false) =>
        s"$fieldName.fold(__NullValue: __Value)(value => ${writeInputValue(ListType(ofType, nonNull = true), "value", typeName, mappingClashedTypeNames)})"
    }

  def writeEnum(typedef: EnumTypeDefinition, mappingClashedTypeNames: Map[String, String]): String = {

    val enumName = safeTypeName(typedef.name, mappingClashedTypeNames)

    val mappingClashedenumValues = getMappingsClashedNames(
      typedef.enumValuesDefinition.map(_.enumValue)
    )

    def safeEnumValue(enumValue: String): String =
      safeName(mappingClashedenumValues.getOrElse(enumValue, enumValue))

    s"""sealed trait $enumName extends scala.Product with scala.Serializable
        object $enumName {
          ${typedef.enumValuesDefinition
      .map(v => s"case object ${safeEnumValue(v.enumValue)} extends $enumName")
      .mkString("\n")}

          implicit val decoder: ScalarDecoder[$enumName] = {
            ${typedef.enumValuesDefinition
      .map(v => s"""case __StringValue ("${v.enumValue}") => Right($enumName.${safeEnumValue(v.enumValue)})""")
      .mkString("\n")}
            case other => Left(DecodingError(s"Can't build ${typedef.name} from input $$other"))
          }
          implicit val encoder: ArgEncoder[${typedef.name}] = new ArgEncoder[${typedef.name}] {
            override def encode(value: ${enumName}): __Value = value match {
              ${typedef.enumValuesDefinition
      .map(v => s"""case ${typedef.name}.${safeEnumValue(v.enumValue)} => __EnumValue("${v.enumValue}")""")
      .mkString("\n")}
            }
            override def typeName: String = "$enumName"
          }
        }
       """
  }

  def writeScalar(typedef: ScalarTypeDefinition, mappingClashedTypeNames: Map[String, String]): String =
    if (typedef.name == "Json") "type Json = io.circe.Json"
    else s"""type ${safeTypeName(typedef.name, mappingClashedTypeNames)} = String
        """

  def safeName(name: String): String =
    if (reservedKeywords.contains(name)) s"`$name`"
    else if (caseClassReservedFields.contains(name)) s"${name}_"
    else name
  @tailrec
  def getTypeLetter(typesMap: Map[String, TypeDefinition], letter: String = "A"): String =
    if (!typesMap.contains(letter)) letter else getTypeLetter(typesMap, letter + "A")

  private val tripleQuotes = "\"\"\""
  private val doubleQuotes = "\""

  def writeField(
    field: FieldDefinition,
    typeName: String,
    typesMap: Map[String, TypeDefinition],
    mappingClashedTypeNames: Map[String, String]
  ): String =
    writeFieldInfo(collectFieldInfo(field, typeName, typesMap, mappingClashedTypeNames))

  def writeFieldInfo(fieldInfo: FieldInfo): String = {
    val FieldInfo(
      name,
      safeName,
      description,
      deprecated,
      typeName,
      typeParam,
      args,
      innerSelection,
      outputType,
      builder,
      argBuilder,
      _
    ) =
      fieldInfo

    s"""$description${deprecated}def $safeName$typeParam$args$innerSelection: SelectionBuilder[$typeName, $outputType] = Field("$name", $builder$argBuilder)"""
  }

  def collectFieldInfo(
    field: FieldDefinition,
    typeName: String,
    typesMap: Map[String, TypeDefinition],
    mappingClashedTypeNames: Map[String, String]
  ): FieldInfo = {
    val name = safeName(field.name)
    val description = field.description match {
      case Some(d) if d.trim.nonEmpty => s"/**\n * ${d.trim}\n */\n"
      case _                          => ""
    }
    val deprecated = field.directives.find(_.name == "deprecated") match {
      case None => ""
      case Some(directive) =>
        val body =
          directive.arguments.collectFirst {
            case ("reason", StringValue(reason)) => reason
          }.getOrElse("")

        val quotes =
          if (body.contains("\n")) tripleQuotes
          else doubleQuotes

        "@deprecated(" + quotes + body + quotes + """, "")""" + "\n"
    }
    val fieldType = safeTypeName(getTypeName(field.ofType), mappingClashedTypeNames)
    val isScalar = typesMap
      .get(fieldType)
      .collect {
        case _: ScalarTypeDefinition => true
        case _: EnumTypeDefinition   => true
        case _                       => false
      }
      .getOrElse(true)
    val unionTypes = typesMap
      .get(fieldType)
      .collect {
        case UnionTypeDefinition(_, _, _, memberTypes) =>
          memberTypes.flatMap(name => typesMap.get(safeTypeName(name, mappingClashedTypeNames)))
      }
      .getOrElse(Nil)
      .collect {
        case o: ObjectTypeDefinition => o
      }
    val interfaceTypes = typesMap
      .get(fieldType)
      .collect {
        case InterfaceTypeDefinition(_, name, _, _) => name
      }
      .map(interface =>
        typesMap.values.collect {
          case o @ ObjectTypeDefinition(_, _, implements, _, _) if implements.exists(_.name == interface) => o
        }
      )
      .getOrElse(Nil)
    val typeLetter = getTypeLetter(typesMap)
    val (typeParam, innerSelection, outputType, builder) =
      if (isScalar) {
        (
          "",
          "",
          writeType(field.ofType, mappingClashedTypeNames),
          writeTypeBuilder(field.ofType, "Scalar()")
        )
      } else if (unionTypes.nonEmpty) {
        (
          s"[$typeLetter]",
          s"(${unionTypes.map(t => s"""on${t.name}: SelectionBuilder[${safeTypeName(t.name, mappingClashedTypeNames)}, $typeLetter]""").mkString(", ")})",
          writeType(field.ofType, mappingClashedTypeNames).replace(fieldType, typeLetter),
          writeTypeBuilder(
            field.ofType,
            s"ChoiceOf(Map(${unionTypes.map(t => s""""${t.name}" -> Obj(on${t.name})""").mkString(", ")}))"
          )
        )
      } else if (interfaceTypes.nonEmpty) {
        (
          s"[$typeLetter]",
          s"(${interfaceTypes.map(t => s"""on${t.name}: Option[SelectionBuilder[${safeTypeName(t.name, mappingClashedTypeNames)}, $typeLetter]] = None""").mkString(", ")})",
          writeType(field.ofType, mappingClashedTypeNames).replace(fieldType, typeLetter),
          writeTypeBuilder(
            field.ofType,
            s"ChoiceOf(Map(${interfaceTypes.map(t => s""""${t.name}" -> on${t.name}""").mkString(", ")}).collect { case (k, Some(v)) => k -> Obj(v)})"
          )
        )
      } else {
        (
          s"[$typeLetter]",
          s"(innerSelection: SelectionBuilder[$fieldType, $typeLetter])",
          writeType(field.ofType, mappingClashedTypeNames).replace(fieldType, typeLetter),
          writeTypeBuilder(field.ofType, "Obj(innerSelection)")
        )
      }
    val args = field.args match {
      case Nil  => ""
      case list => s"(${writeArgumentFields(list, mappingClashedTypeNames)})"
    }
    val argBuilder = field.args match {
      case Nil => ""
      case list =>
        s", arguments = List(${list.map(arg => s"""Argument("${arg.name}", ${safeName(arg.name)})""").mkString(", ")})"
    }

    val owner         = if (typeParam.nonEmpty) Some(fieldType) else None
    val fieldTypeInfo = FieldTypeInfo(name, outputType, field.args, owner)
    FieldInfo(
      field.name,
      name,
      description,
      deprecated,
      typeName,
      typeParam,
      args,
      innerSelection,
      outputType,
      builder,
      argBuilder,
      fieldTypeInfo
    )
  }

  def writeArgumentFields(args: List[InputValueDefinition], mappingClashedTypeNames: Map[String, String]): String =
    s"${args.map(arg => s"${safeName(arg.name)} : ${writeType(arg.ofType, mappingClashedTypeNames)}${writeDefaultArgument(arg)}").mkString(", ")}"

  def writeDefaultArgument(arg: InputValueDefinition): String =
    arg.ofType match {
      case t if t.nullable => " = None"
      case ListType(_, _)  => " = Nil"
      case _               => ""
    }

  def writeDescription(description: Option[String]): String =
    description.fold("")(d => s"""@GQLDescription("$d")
                                 |""".stripMargin)

  def mapTypeName(s: String, mappingClashedTypeNames: Map[String, String]): String = s match {
    case "Float" => "Double"
    case "ID"    => "String"
    case other   => safeTypeName(other, mappingClashedTypeNames)
  }

  def writeType(t: Type, mappingClashedTypeNames: Map[String, String]): String =
    t match {
      case NamedType(name, true)   => mapTypeName(name, mappingClashedTypeNames)
      case NamedType(name, false)  => s"Option[${mapTypeName(name, mappingClashedTypeNames)}]"
      case ListType(ofType, true)  => s"List[${writeType(ofType, mappingClashedTypeNames)}]"
      case ListType(ofType, false) => s"Option[List[${writeType(ofType, mappingClashedTypeNames)}]]"
    }

  def writeTypeBuilder(t: Type, inner: String): String =
    t match {
      case NamedType(_, true)  => inner
      case NamedType(_, false) => s"OptionOf($inner)"
      case ListType(of, true)  => s"ListOf(${writeTypeBuilder(of, inner)})"
      case ListType(of, false) => s"OptionOf(ListOf(${writeTypeBuilder(of, inner)}))"
    }

  @tailrec
  def getTypeName(t: Type): String = t match {
    case NamedType(name, _)  => name
    case ListType(ofType, _) => getTypeName(ofType)
  }

  val supportedScalars =
    Set("Int", "Float", "Double", "Long", "Unit", "String", "Boolean", "BigInt", "BigDecimal")

  val reservedKeywords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
    "_"
  )

  val caseClassReservedFields =
    Set("wait", "notify", "toString", "notifyAll", "hashCode", "getClass", "finalize", "equals", "clone")

  final case class FieldTypeInfo(
    name: String,
    outputType: String,
    arguments: List[InputValueDefinition],
    owner: Option[String]
  )

  final case class FieldInfo(
    name: String,
    safeName: String,
    description: String,
    deprecated: String,
    typeName: String,
    typeParam: String,
    args: String,
    innerSelection: String,
    outputType: String,
    builder: String,
    argBuilder: String,
    typeInfo: FieldTypeInfo
  )
}
