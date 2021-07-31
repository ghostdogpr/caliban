package caliban.tools

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }
import caliban.tools.implicits.Implicits._
import caliban.tools.implicits.{ MappingClashedTypeNames, ScalarMappings, TypesMap }

import scala.annotation.tailrec

object ClientWriter {

  private val MaxTupleLength = 22

  def write(
    schema: Document,
    objectName: String = "Client",
    packageName: Option[String] = None,
    genView: Boolean = false,
    additionalImports: Option[List[String]] = None,
    splitFiles: Boolean = false,
    extensibleEnums: Boolean = false
  )(implicit scalarMappings: ScalarMappings): List[(String, String)] = {
    require(packageName.isDefined || !splitFiles, "splitFiles option requires a package name")

    val schemaDef = schema.schemaDefinition

    implicit val mappingClashedTypeNames: MappingClashedTypeNames = MappingClashedTypeNames(
      getMappingsClashedNames(
        schema.definitions.collect {
          case ObjectTypeDefinition(_, name, _, _, _)   => name
          case InputObjectTypeDefinition(_, name, _, _) => name
          case EnumTypeDefinition(_, name, _, _)        => name
          case UnionTypeDefinition(_, name, _, _)       => name
          case ScalarTypeDefinition(_, name, _)         => name
          case InterfaceTypeDefinition(_, name, _, _)   => name
        },
        if (splitFiles) List("package") else Nil
      )
    )

    implicit val typesMap: TypesMap = TypesMap(schema.definitions.collect {
      case op @ ObjectTypeDefinition(_, name, _, _, _)   => name -> op
      case op @ InputObjectTypeDefinition(_, name, _, _) => name -> op
      case op @ EnumTypeDefinition(_, name, _, _)        => name -> op
      case op @ UnionTypeDefinition(_, name, _, _)       => name -> op
      case op @ ScalarTypeDefinition(_, name, _)         => name -> op
      case op @ InterfaceTypeDefinition(_, name, _, _)   => name -> op
    }.map { case (name, op) =>
      safeTypeName(name) -> op
    }.toMap)

    val objectTypes =
      if (splitFiles)
        schema.objectTypeDefinitions
          .filterNot(obj =>
            reservedType(obj) ||
              schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
              schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
              schemaDef.exists(_.subscription.getOrElse("Subscription") == obj.name)
          )
          .map(writeObjectType)
      else Nil

    val objects = schema.objectTypeDefinitions
      .filterNot(obj =>
        reservedType(obj) ||
          schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
          schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
          schemaDef.exists(_.subscription.getOrElse("Subscription") == obj.name)
      )
      .map { typedef =>
        val content     = writeObject(typedef, genView)
        val fullContent =
          if (splitFiles)
            s"""import caliban.client.FieldBuilder._
               |import caliban.client._
               |
               |$content
               |""".stripMargin
          else
            s"""${writeObjectType(typedef)}
               |$content
               |""".stripMargin
        safeTypeName(typedef.name) -> fullContent
      }

    val inputs = schema.inputObjectTypeDefinitions.map { typedef =>
      val content     = writeInputObject(typedef)
      val fullContent =
        if (splitFiles)
          s"""import caliban.client._
             |import caliban.client.__Value._
             |
             |$content
             |""".stripMargin
        else
          content
      safeTypeName(typedef.name) -> fullContent
    }

    val enums = schema.enumTypeDefinitions
      .filter(e => !scalarMappings.scalarMap.exists(_.contains(e.name)))
      .map { typedef =>
        val content     = writeEnum(typedef, extensibleEnums = extensibleEnums)
        val fullContent =
          if (splitFiles)
            s"""import caliban.client.CalibanClientError.DecodingError
               |import caliban.client._
               |import caliban.client.__Value._
               |
               |$content
               |""".stripMargin
          else
            content
        safeTypeName(typedef.name) -> fullContent
      }

    val scalars = schema.scalarTypeDefinitions
      .filterNot(s => isScalarSupported(s.name))
      .map(writeScalar)

    val queryTypes =
      if (splitFiles)
        schema
          .objectTypeDefinition(schemaDef.flatMap(_.query).getOrElse("Query"))
          .map(writeRootQueryType)
          .toList
      else Nil

    val queries = schema
      .objectTypeDefinition(schemaDef.flatMap(_.query).getOrElse("Query"))
      .map { typedef =>
        val content     = writeRootQuery(typedef)
        val fullContent =
          if (splitFiles)
            s"""import caliban.client.FieldBuilder._
               |import caliban.client._
               |
               |$content
               |""".stripMargin
          else
            s"""${writeRootQueryType(typedef)}
               |$content
               |""".stripMargin
        safeTypeName(typedef.name) -> fullContent
      }

    val mutationTypes =
      if (splitFiles)
        schema
          .objectTypeDefinition(schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
          .map(writeRootMutationType)
          .toList
      else Nil

    val mutations = schema
      .objectTypeDefinition(schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
      .map { typedef =>
        val content     = writeRootMutation(typedef)
        val fullContent =
          if (splitFiles)
            s"""import caliban.client.FieldBuilder._
               |import caliban.client._
               |
               |$content
               |""".stripMargin
          else
            s"""${writeRootMutationType(typedef)}
               |$content
               |""".stripMargin
        safeTypeName(typedef.name) -> fullContent
      }

    val subscriptionTypes =
      if (splitFiles)
        schema
          .objectTypeDefinition(schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
          .map(writeRootSubscriptionType)
          .toList
      else Nil

    val subscriptions = schema
      .objectTypeDefinition(schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
      .map { typedef =>
        val content     = writeRootSubscription(typedef)
        val fullContent =
          if (splitFiles)
            s"""import caliban.client.FieldBuilder._
               |import caliban.client._
               |
               |$content
               |""".stripMargin
          else
            s"""${writeRootSubscriptionType(typedef)}
               |$content
               |""".stripMargin
        safeTypeName(typedef.name) -> fullContent
      }

    val additionalImportsString = additionalImports.fold("")(_.map(i => s"import $i").mkString("\n"))

    if (splitFiles) {
      val parentPackageName = packageName.filter(_.contains(".")).map(_.reverse.dropWhile(_ != '.').drop(1).reverse)
      val packageObject     = "package" ->
        s"""${parentPackageName.fold("")(p => s"package $p\n")}
           |$additionalImportsString
           |
           |package object ${packageName.get.reverse.takeWhile(_ != '.').reverse} {
           |  ${(scalars ::: objectTypes ::: queryTypes ::: mutationTypes ::: subscriptionTypes).mkString("\n")}
           |}
           |""".stripMargin
      val classFiles        =
        (enums ::: objects ::: inputs ::: queries.toList ::: mutations.toList ::: subscriptions.toList).map {
          case (name, content) =>
            val fullContent =
              s"""${packageName.fold("")(p => s"package $p\n\n")}
                 |$additionalImportsString
                 |
                 |$content
                 |""".stripMargin
            name -> fullContent
        }
      packageObject :: classFiles
    } else {
      val imports =
        s"""${if (enums.nonEmpty)
          """import caliban.client.CalibanClientError.DecodingError
            |""".stripMargin
        else ""}${if (objects.nonEmpty || queries.nonEmpty || mutations.nonEmpty || subscriptions.nonEmpty)
          """import caliban.client.FieldBuilder._
            |""".stripMargin
        else
          ""}${if (
          enums.nonEmpty || objects.nonEmpty || queries.nonEmpty || mutations.nonEmpty || subscriptions.nonEmpty || inputs.nonEmpty
        )
          """import caliban.client._
            |""".stripMargin
        else ""}${if (enums.nonEmpty || inputs.nonEmpty)
          """import caliban.client.__Value._
            |""".stripMargin
        else ""}"""

      List(objectName -> s"""${packageName.fold("")(p => s"package $p\n\n")}$imports\n
                            |$additionalImportsString
                            |
                            |object $objectName {
                            |
                            |  ${scalars.mkString("\n")}
                            |  ${enums.map(_._2).mkString("\n")}
                            |  ${objects.map(_._2).mkString("\n")}
                            |  ${inputs.map(_._2).mkString("\n")}
                            |  ${queries.map(_._2).mkString("\n")}
                            |  ${mutations.map(_._2).mkString("\n")}
                            |  ${subscriptions.map(_._2).mkString("\n")}
                            |
                            |}""".stripMargin)
    }
  }

  private def getMappingsClashedNames(typeNames: List[String], reservedNames: List[String] = Nil): Map[String, String] =
    (reservedNames ::: typeNames)
      .map(name => name.toLowerCase -> name)
      .groupBy(_._1)
      .collect {
        case (_, _ :: typeNamesToRename) if typeNamesToRename.nonEmpty =>
          typeNamesToRename.zipWithIndex.map { case ((_, originalTypeName), index) =>
            val suffix = "_" + (index + 1)
            originalTypeName -> s"$originalTypeName$suffix"
          }.toMap
      }
      .reduceOption(_ ++ _)
      .getOrElse(Map.empty)

  private def safeTypeName(
    typeName: String
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String =
    mappingClashedTypeNames.getOrElse(
      typeName,
      scalarMappings.scalarMap.flatMap(m => m.get(typeName)).getOrElse(safeName(typeName))
    )

  def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
    typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

  def writeRootQueryType(
    typedef: ObjectTypeDefinition
  ): String =
    s"type ${typedef.name} = _root_.caliban.client.Operations.RootQuery"

  def writeRootQuery(
    typedef: ObjectTypeDefinition
  )(implicit
    typesMap: TypesMap,
    mappingClashedTypeNames: MappingClashedTypeNames,
    scalarMappings: ScalarMappings
  ): String =
    s"""object ${typedef.name} {
       |  ${typedef.fields.map(writeField(_, "_root_.caliban.client.Operations.RootQuery")).mkString("\n  ")}
       |}
       |""".stripMargin

  def writeRootMutationType(
    typedef: ObjectTypeDefinition
  ): String =
    s"type ${typedef.name} = _root_.caliban.client.Operations.RootMutation"

  def writeRootMutation(
    typedef: ObjectTypeDefinition
  )(implicit
    typesMap: TypesMap,
    mappingClashedTypeNames: MappingClashedTypeNames,
    scalarMappings: ScalarMappings
  ): String =
    s"""object ${typedef.name} {
       |  ${typedef.fields.map(writeField(_, "_root_.caliban.client.Operations.RootMutation")).mkString("\n  ")}
       |}
       |""".stripMargin

  def writeRootSubscriptionType(
    typedef: ObjectTypeDefinition
  ): String =
    s"type ${typedef.name} = _root_.caliban.client.Operations.RootSubscription"

  def writeRootSubscription(
    typedef: ObjectTypeDefinition
  )(implicit
    typesMap: TypesMap,
    mappingClashedTypeNames: MappingClashedTypeNames,
    scalarMappings: ScalarMappings
  ): String =
    s"""object ${typedef.name} {
       |  ${typedef.fields.map(writeField(_, "_root_.caliban.client.Operations.RootSubscription")).mkString("\n  ")}
       |}
       |""".stripMargin

  def writeObjectType(
    typedef: ObjectTypeDefinition
  )(implicit
    mappingClashedTypeNames: MappingClashedTypeNames,
    scalarMappings: ScalarMappings
  ): String = {

    val objectName: String = safeTypeName(typedef.name)

    s"type $objectName"
  }

  def writeObject(
    typedef: ObjectTypeDefinition,
    genView: Boolean
  )(implicit
    typesMap: TypesMap,
    mappingClashedTypeNames: MappingClashedTypeNames,
    scalarMappings: ScalarMappings
  ): String = {

    val objectName: String = safeTypeName(typedef.name)
    val fields             = typedef.fields.map(collectFieldInfo(_, objectName))
    val view               =
      if (genView)
        "\n  " + writeView(typedef.name, fields.map(_.typeInfo))
      else ""

    s"""object $objectName {$view
       |  ${fields.map(writeFieldInfo).mkString("\n  ")}
       |}
       |""".stripMargin
  }

  def writeView(
    objectName: String,
    fields: List[FieldTypeInfo]
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String = {
    val viewName       = s"${objectName}View"
    val safeObjectName = safeTypeName(objectName)

    def argumentName(fieldName: String, argName: String): String =
      fieldName + argName.capitalize

    def withRoundBrackets(input: List[String]): String =
      if (input.nonEmpty) input.mkString("(", ", ", ")") else ""

    val genericSelectionFields =
      fields.collect {
        case field if field.owner.nonEmpty =>
          field -> s"${field.rawName}Selection"
      }

    val genericSelectionFieldTypes    =
      genericSelectionFields.map { case (field, name) => (field, name.capitalize) }

    val genericSelectionFieldsMap     = genericSelectionFields.toMap
    val genericSelectionFieldTypesMap = genericSelectionFieldTypes.toMap

    val viewFunctionArgumentsCount: Int     = fields.map(_.arguments.length).sum
    val needsCaseClassForArguments          = viewFunctionArgumentsCount > MaxTupleLength
    val viewFunctionArguments: List[String] =
      fields.collect {
        case field if field.arguments.nonEmpty =>
          writeArgumentFields(
            field.arguments.map(a => a.copy(name = argumentName(field.name, a.name)))
          )
      }

    val viewFunctionSelectionArgumentsCount: Int     = genericSelectionFields.collect {
      case (FieldTypeInfo(_, _, _, Nil, Nil, _, Some(_)), _)                                     => 1
      case (FieldTypeInfo(_, _, _, _, unionTypes, _, Some(_)), _) if unionTypes.nonEmpty         => unionTypes.length
      case (FieldTypeInfo(_, _, _, interfaceTypes, _, _, Some(_)), _) if interfaceTypes.nonEmpty =>
        interfaceTypes.length
    }.sum
    val needsCaseClassForSelectionArguments          = viewFunctionSelectionArgumentsCount > MaxTupleLength
    val viewFunctionSelectionArguments: List[String] =
      genericSelectionFields.collect {
        case (field @ FieldTypeInfo(_, _, _, Nil, Nil, _, Some(owner)), fieldName) =>
          val tpe = genericSelectionFieldTypesMap(field)
          List(s"$fieldName: SelectionBuilder[$owner, $tpe]")

        case (field @ FieldTypeInfo(_, _, _, _, unionTypes, _, Some(_)), fieldName) if unionTypes.nonEmpty =>
          val tpe = genericSelectionFieldTypesMap(field)
          unionTypes.map(unionType => s"${fieldName}On$unionType: SelectionBuilder[$unionType, $tpe]")

        case (field @ FieldTypeInfo(_, _, _, interfaceTypes, _, _, Some(_)), fieldName) if interfaceTypes.nonEmpty =>
          val tpe = genericSelectionFieldTypesMap(field)
          interfaceTypes.map(intType => s"${fieldName}On$intType: Option[SelectionBuilder[$intType, $tpe]] = None")
      }.flatten

    val viewClassFields: List[String] =
      fields.map {
        case field @ FieldTypeInfo(_, _, outputType, _, _, _, Some(_)) =>
          val tpeName = genericSelectionFieldTypesMap(field)
          val tpe     =
            if (outputType.contains("[A]")) outputType.replace("[A]", s"[$tpeName]")
            else outputType.dropRight(1) + tpeName
          s"${field.name}: $tpe"

        case field @ FieldTypeInfo(_, _, outputType, _, _, _, _) =>
          s"${field.name}: $outputType"
      }

    val viewFunctionBody: String =
      fields.map { case field @ FieldTypeInfo(_, _, _, interfaceTypes, unionTypes, _, _) =>
        val argsPart      = withRoundBrackets(
          field.arguments
            .map(a => argumentName(field.name, a.name))
            .map(name => if (needsCaseClassForArguments) s"args.$name" else name)
        )
        val selectionType = genericSelectionFieldsMap
          .get(field)
          .map(name => if (needsCaseClassForSelectionArguments) s"selectionArgs.$name" else name)
        val selectionPart = {
          val parts =
            if (unionTypes.nonEmpty) unionTypes.map(unionType => s"${selectionType.head}On$unionType")
            else if (interfaceTypes.nonEmpty) interfaceTypes.map(tpe => s"${selectionType.head}On$tpe")
            else selectionType.toList

          withRoundBrackets(parts)
        }

        s"${field.name}$argsPart$selectionPart"
      }.mkString(" ~ ")

    val viewClassFieldParams: String = withRoundBrackets(viewClassFields)

    val viewFunction: String =
      fields match {
        case head :: Nil =>
          s"$viewFunctionBody.map(${head.name} => $viewName(${head.name}))"

        case other =>
          val unapply = fields.tail.foldLeft(safeUnapplyName(fields.head.rawName)) { case (acc, field) =>
            "(" + acc + ", " + safeUnapplyName(field.rawName) + ")"
          }
          s"($viewFunctionBody).map { case $unapply => $viewName(${other.map(f => safeUnapplyName(f.rawName)).mkString(", ")}) }"
      }

    val typeParams =
      if (genericSelectionFieldTypes.nonEmpty) genericSelectionFieldTypes.map(_._2).mkString("[", ", ", "]") else ""

    val viewFunctionArgs          =
      if (needsCaseClassForArguments) s"(args: ${viewName}Args)" else withRoundBrackets(viewFunctionArguments)
    val viewFunctionSelectionArgs =
      if (needsCaseClassForSelectionArguments) s"(selectionArgs: ${viewName}SelectionArgs$typeParams)"
      else withRoundBrackets(viewFunctionSelectionArguments)

    val caseClassForArguments          =
      if (needsCaseClassForArguments) s"final case class ${viewName}Args${withRoundBrackets(viewFunctionArguments)}"
      else ""
    val caseClassForSelectionArguments =
      if (needsCaseClassForSelectionArguments)
        s"final case class ${viewName}SelectionArgs$typeParams${withRoundBrackets(viewFunctionSelectionArguments)}"
      else ""

    s"""
       |final case class $viewName$typeParams$viewClassFieldParams
       |
       |$caseClassForArguments
       |$caseClassForSelectionArguments
       |
       |type ViewSelection$typeParams = SelectionBuilder[$safeObjectName, $viewName$typeParams]
       |
       |def view$typeParams$viewFunctionArgs$viewFunctionSelectionArgs: ViewSelection$typeParams = $viewFunction
       |""".stripMargin
  }

  def writeInputObject(
    typedef: InputObjectTypeDefinition
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String = {
    val inputObjectName = safeTypeName(typedef.name)
    s"""case class $inputObjectName(${writeArgumentFields(typedef.fields)})
       |object $inputObjectName {
       |  implicit val encoder: ArgEncoder[$inputObjectName] = new ArgEncoder[$inputObjectName] {
       |    override def encode(value: $inputObjectName): __Value =
       |      __ObjectValue(List(${typedef.fields
      .map(f =>
        s""""${f.name}" -> ${writeInputValue(
             f.ofType,
             s"value.${safeName(f.name)}",
             inputObjectName
           )}"""
      )
      .mkString(", ")}))
       |  }
       |}""".stripMargin
  }

  def writeInputValue(
    t: Type,
    fieldName: String,
    typeName: String
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String = t match {
    case NamedType(name, true)   =>
      if (name == typeName) s"encode($fieldName)"
      else s"implicitly[ArgEncoder[${mapTypeName(name)}]].encode($fieldName)"
    case NamedType(name, false)  =>
      s"$fieldName.fold(__NullValue: __Value)(value => ${writeInputValue(NamedType(name, nonNull = true), "value", typeName)})"
    case ListType(ofType, true)  =>
      s"__ListValue($fieldName.map(value => ${writeInputValue(ofType, "value", typeName)}))"
    case ListType(ofType, false) =>
      s"$fieldName.fold(__NullValue: __Value)(value => ${writeInputValue(ListType(ofType, nonNull = true), "value", typeName)})"
  }

  def writeEnum(
    typedef: EnumTypeDefinition,
    extensibleEnums: Boolean
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String = {

    val enumName = safeTypeName(typedef.name)

    val mappingClashedEnumValues = getMappingsClashedNames(
      typedef.enumValuesDefinition.map(_.enumValue)
    )

    def safeEnumValue(enumValue: String): String =
      safeName(mappingClashedEnumValues.getOrElse(enumValue, enumValue))

    val enumCases = typedef.enumValuesDefinition
      .map(v =>
        s"case object ${safeEnumValue(v.enumValue)} extends $enumName { val value: String = ${"\"" + safeEnumValue(v.enumValue) + "\""} }"
      ) ++
      (if (extensibleEnums) Some(s"case class __Unknown(value: String) extends $enumName") else None)

    val decoderCases = typedef.enumValuesDefinition
      .map(v => s"""case __StringValue ("${v.enumValue}") => Right($enumName.${safeEnumValue(v.enumValue)})""") ++
      (if (extensibleEnums) Some(s"case __StringValue (other) => Right($enumName.__Unknown(other))") else None)

    val encoderCases = typedef.enumValuesDefinition
      .map(v => s"""case ${typedef.name}.${safeEnumValue(v.enumValue)} => __EnumValue("${v.enumValue}")""") ++
      (if (extensibleEnums) Some(s"case ${typedef.name}.__Unknown (value) => __EnumValue(value)") else None)

    s"""sealed trait $enumName extends scala.Product with scala.Serializable { def value: String }
        object $enumName {
          ${enumCases.mkString("\n")}

          implicit val decoder: ScalarDecoder[$enumName] = {
            ${decoderCases.mkString("\n")}
            case other => Left(DecodingError(s"Can't build ${typedef.name} from input $$other"))
          }
          implicit val encoder: ArgEncoder[${typedef.name}] = {
            ${encoderCases.mkString("\n")}
          }

          val values: Vector[${enumName}] = Vector(${typedef.enumValuesDefinition
      .map(v => safeEnumValue(v.enumValue))
      .mkString(", ")})
        }
       """
  }

  def writeScalar(
    typedef: ScalarTypeDefinition
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String =
    s"""type ${safeTypeName(typedef.name)} = String
        """

  def safeUnapplyName(name: String): String =
    if (reservedKeywords.contains(name) || name.endsWith("_")) s"$name$$"
    else name

  def safeName(name: String): String =
    if (reservedKeywords.contains(name) || name.endsWith("_")) s"`$name`"
    else if (caseClassReservedFields.contains(name)) s"$name$$"
    else name

  @tailrec
  def getTypeLetter(typesMap: Map[String, TypeDefinition], letter: String = "A"): String =
    if (!typesMap.contains(letter)) letter else getTypeLetter(typesMap, letter + "A")

  private val tripleQuotes = "\"\"\""
  private val doubleQuotes = "\""

  def writeField(
    field: FieldDefinition,
    typeName: String
  )(implicit
    typesMap: TypesMap,
    mappingClashedTypeNames: MappingClashedTypeNames,
    scalarMappings: ScalarMappings
  ): String =
    writeFieldInfo(collectFieldInfo(field, typeName))

  def writeFieldInfo(fieldInfo: FieldInfo): String = {
    val FieldInfo(
      name,
      safeName,
      description,
      deprecated,
      typeName,
      typeParam,
      args,
      implicits,
      innerSelection,
      outputType,
      builder,
      argBuilder,
      _
    ) =
      fieldInfo

    s"""$description${deprecated}def $safeName$typeParam$args$innerSelection$implicits: SelectionBuilder[$typeName, $outputType] = _root_.caliban.client.SelectionBuilder.Field("$name", $builder$argBuilder)"""
  }

  def collectFieldInfo(
    field: FieldDefinition,
    typeName: String
  )(implicit
    typesMap: TypesMap,
    mappingClashedTypeNames: MappingClashedTypeNames,
    scalarMappings: ScalarMappings
  ): FieldInfo = {
    val name                                             = safeName(field.name)
    val description                                      = field.description match {
      case Some(d) if d.trim.nonEmpty => s"/**\n * ${d.trim}\n */\n"
      case _                          => ""
    }
    val deprecated                                       = field.directives.find(_.name == "deprecated") match {
      case None            => ""
      case Some(directive) =>
        val body =
          directive.arguments.collectFirst { case ("reason", StringValue(reason)) =>
            reason
          }.getOrElse("")

        val quotes =
          if (body.contains("\n")) tripleQuotes
          else doubleQuotes

        "@deprecated(" + quotes + body + quotes + """, "")""" + "\n"
    }
    val fieldType                                        = safeTypeName(getTypeName(field.ofType))
    val isScalar                                         = typesMap
      .get(fieldType)
      .collect {
        case _: ScalarTypeDefinition => true
        case _: EnumTypeDefinition   => true
        case _                       => false
      }
      .getOrElse(true)
    val unionTypes                                       = typesMap
      .get(fieldType)
      .collect { case UnionTypeDefinition(_, _, _, memberTypes) =>
        memberTypes.flatMap(name => typesMap.get(safeTypeName(name)))
      }
      .getOrElse(Nil)
      .collect { case o: ObjectTypeDefinition =>
        o
      }
    val interfaceTypes                                   = typesMap
      .get(fieldType)
      .collect { case InterfaceTypeDefinition(_, name, _, _) =>
        name
      }
      .map(interface =>
        typesMap.values.collect {
          case o @ ObjectTypeDefinition(_, _, implements, _, _) if implements.exists(_.name == interface) => o
        }
      )
      .getOrElse(Nil)
    val typeLetter                                       = getTypeLetter(typesMap)
    val (typeParam, innerSelection, outputType, builder) =
      if (isScalar) {
        (
          "",
          "",
          writeType(field.ofType),
          writeTypeBuilder(field.ofType, "Scalar()")
        )
      } else if (unionTypes.nonEmpty) {
        (
          s"[$typeLetter]",
          s"(${unionTypes.map(t => s"""on${t.name}: SelectionBuilder[${safeTypeName(t.name)}, $typeLetter]""").mkString(", ")})",
          writeType(field.ofType).replace(fieldType, typeLetter),
          writeTypeBuilder(
            field.ofType,
            s"ChoiceOf(Map(${unionTypes.map(t => s""""${t.name}" -> Obj(on${t.name})""").mkString(", ")}))"
          )
        )
      } else if (interfaceTypes.nonEmpty) {
        (
          s"[$typeLetter]",
          s"(${interfaceTypes.map(t => s"""on${t.name}: Option[SelectionBuilder[${safeTypeName(t.name)}, $typeLetter]] = None""").mkString(", ")})",
          writeType(field.ofType).replace(fieldType, typeLetter),
          writeTypeBuilder(
            field.ofType,
            s"ChoiceOf(Map(${interfaceTypes.map(t => s""""${t.name}" -> on${t.name}""").mkString(", ")}).collect { case (k, Some(v)) => k -> Obj(v)})"
          )
        )
      } else {
        (
          s"[$typeLetter]",
          s"(innerSelection: SelectionBuilder[$fieldType, $typeLetter])",
          writeType(field.ofType).replace(fieldType, typeLetter),
          writeTypeBuilder(field.ofType, "Obj(innerSelection)")
        )
      }
    val args                                             = field.args match {
      case Nil  => ""
      case list => s"(${writeArgumentFields(list)})"
    }
    val argBuilder                                       = field.args match {
      case Nil  => ""
      case list =>
        s", arguments = List(${list.zipWithIndex.map { case (arg, idx) =>
          s"""Argument("${arg.name}", ${safeName(arg.name)}, "${arg.ofType.toString}")(encoder$idx)"""
        }.mkString(", ")})"
    }
    val implicits                                        = field.args match {
      case Nil  => ""
      case list =>
        s"(implicit ${list.zipWithIndex.map { case (arg, idx) =>
          s"""encoder$idx: ArgEncoder[${writeType(arg.ofType)}]"""
        }.mkString(", ")})"
    }

    val owner         = if (typeParam.nonEmpty) Some(fieldType) else None
    val fieldTypeInfo = FieldTypeInfo(
      field.name,
      name,
      outputType,
      interfaceTypes.map(_.name).toList,
      unionTypes.map(_.name),
      field.args,
      owner
    )
    FieldInfo(
      field.name,
      name,
      description,
      deprecated,
      typeName,
      typeParam,
      args,
      implicits,
      innerSelection,
      outputType,
      builder,
      argBuilder,
      fieldTypeInfo
    )
  }

  def writeArgumentFields(
    args: List[InputValueDefinition]
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String =
    s"${args.map(arg => s"${safeName(arg.name)} : ${writeType(arg.ofType)}${writeDefaultArgument(arg)}").mkString(", ")}"

  def writeDefaultArgument(arg: InputValueDefinition): String =
    arg.ofType match {
      case t if t.nullable => " = None"
      case ListType(_, _)  => " = Nil"
      case _               => ""
    }

  def writeDescription(description: Option[String]): String =
    description.fold("")(d => s"""@GQLDescription("$d")
                                 |""".stripMargin)

  def mapTypeName(
    s: String
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String = s match {
    case "Float" => "Double"
    case "ID"    => "String"
    case other   => safeTypeName(other)
  }

  def writeType(
    t: Type
  )(implicit mappingClashedTypeNames: MappingClashedTypeNames, scalarMappings: ScalarMappings): String = t match {
    case NamedType(name, true)   => mapTypeName(name)
    case NamedType(name, false)  => s"Option[${mapTypeName(name)}]"
    case ListType(ofType, true)  => s"List[${writeType(ofType)}]"
    case ListType(ofType, false) => s"Option[List[${writeType(ofType)}]]"
  }

  def writeTypeBuilder(t: Type, inner: String): String = t match {
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

  def isScalarSupported(scalar: String)(implicit scalarMappings: ScalarMappings): Boolean =
    supportedScalars.contains(scalar) || scalarMappings.map(_.contains(scalar)).getOrElse(false)

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
    rawName: String,
    name: String,
    outputType: String,
    interfaceTypes: List[String],
    unionTypes: List[String],
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
    implicits: String,
    innerSelection: String,
    outputType: String,
    builder: String,
    argBuilder: String,
    typeInfo: FieldTypeInfo
  )

}
