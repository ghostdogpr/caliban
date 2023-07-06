package caliban.tools

import caliban.Value.StringValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

import scala.annotation.tailrec

object ClientWriter {

  val MaxTupleLength = 22

  def write(
    schema: Document,
    objectName: String = "Client",
    packageName: Option[String] = None,
    genView: Boolean = false,
    additionalImports: Option[List[String]] = None,
    splitFiles: Boolean = false,
    extensibleEnums: Boolean = false,
    scalarMappings: Option[Map[String, String]] = None
  ): List[(String, String)] = {
    require(packageName.isDefined || !splitFiles, "splitFiles option requires a package name")

    val defaultScalarMappings      = Map("Float" -> "Double", "ID" -> "String")
    val scalarMappingsWithDefaults = scalarMappings.fold(defaultScalarMappings)(defaultScalarMappings ++ _)

    def getMappingsClashedNames(typeNames: List[String], reservedNames: List[String] = Nil): Map[String, String] =
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

    val mappingClashedTypeNames: Map[String, String] = getMappingsClashedNames(
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

    def safeUnapplyName(name: String): String =
      if (reservedKeywords.contains(name) || name.endsWith("_") || isCapital(name)) s"${decapitalize(name)}$$"
      else name

    def isCapital(name: String): Boolean = name.nonEmpty && name.charAt(0).isUpper

    def decapitalize(name: String): String = if (isCapital(name)) {
      val chars = name.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    } else {
      name
    }

    def safeTypeName(typeName: String): String =
      mappingClashedTypeNames.getOrElse(typeName, scalarMappingsWithDefaults.getOrElse(typeName, safeName(typeName)))

    val typesMap: Map[String, TypeDefinition] = schema.definitions.collect {
      case op @ ObjectTypeDefinition(_, name, _, _, _)   => name -> op
      case op @ InputObjectTypeDefinition(_, name, _, _) => name -> op
      case op @ EnumTypeDefinition(_, name, _, _)        => name -> op
      case op @ UnionTypeDefinition(_, name, _, _)       => name -> op
      case op @ ScalarTypeDefinition(_, name, _)         => name -> op
      case op @ InterfaceTypeDefinition(_, name, _, _)   => name -> op
    }.map { case (name, op) =>
      safeTypeName(name) -> op
    }.toMap

    val knownInterfaceTypes = typesMap.collect { case (key, _: InterfaceTypeDefinition) => key }
    val knownUnionTypes     = typesMap.collect { case (key, _: UnionTypeDefinition) => key }

    def isOptionalInterfaceType(field: FieldDefinition): Boolean =
      knownInterfaceTypes.exists(_.compareToIgnoreCase(Type.innerType(field.ofType)) == 0)

    def isOptionalUnionType(field: FieldDefinition): Boolean =
      knownUnionTypes.exists(_.compareToIgnoreCase(Type.innerType(field.ofType)) == 0)

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
      typeName: String,
      optionalUnion: Boolean,
      optionalInterface: Boolean,
      commonInterface: Boolean
    ): FieldInfo = {
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
        .collect { case o: ObjectTypeDefinition => o }
        .sortBy(_.name)
      val interfaceTypes                                   = typesMap
        .get(fieldType)
        .collect { case InterfaceTypeDefinition(_, name, _, _) => name }
        .map(interface =>
          typesMap.values.collect {
            case o @ ObjectTypeDefinition(_, _, implements, _, _) if implements.exists(_.name == interface) => o
          }
        )
        .getOrElse(Nil)
        .toList
        .sortBy(_.name)
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
          if (optionalUnion) {
            (
              s"[$typeLetter]",
              s"(${unionTypes.map(t => s"""on${t.name}: scala.Option[SelectionBuilder[${safeTypeName(t.name)}, $typeLetter]] = None""").mkString(", ")})",
              s"${writeType(field.ofType).replace(fieldType, s"scala.Option[$typeLetter]")}",
              writeTypeBuilder(
                field.ofType,
                s"ChoiceOf(Map(${unionTypes.map(t => s""""${t.name}" -> on${t.name}.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))""").mkString(", ")}))"
              )
            )
          } else {
            (
              s"[$typeLetter]",
              s"(${unionTypes.map(t => s"""on${t.name}: SelectionBuilder[${safeTypeName(t.name)}, $typeLetter]""").mkString(", ")})",
              writeType(field.ofType).replace(fieldType, typeLetter),
              writeTypeBuilder(
                field.ofType,
                s"ChoiceOf(Map(${unionTypes.map(t => s""""${t.name}" -> Obj(on${t.name})""").mkString(", ")}))"
              )
            )
          }
        } else if (interfaceTypes.nonEmpty) {
          if (commonInterface) {
            (
              s"[$typeLetter]",
              s"(${safeName(field.name)}: SelectionBuilder[${safeTypeName(getTypeName(field.ofType))}, $typeLetter])",
              writeType(field.ofType).replace(fieldType, typeLetter),
              writeTypeBuilder(field.ofType, s"Obj(${safeName(field.name)})")
            )
          } else if (optionalInterface) {
            (
              s"[$typeLetter]",
              s"(${interfaceTypes.map(t => s"""on${t.name}: scala.Option[SelectionBuilder[${safeTypeName(t.name)}, $typeLetter]] = None""").mkString(", ")})",
              s"${writeType(field.ofType).replace(fieldType, s"""scala.Option[$typeLetter]""")}",
              writeTypeBuilder(
                field.ofType,
                s"ChoiceOf(Map(${interfaceTypes.map(t => s""""${t.name}" -> on${t.name}.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))""").mkString(", ")}))"
              )
            )
          } else {
            (
              s"[$typeLetter]",
              s"(${interfaceTypes.map(t => s"""on${t.name}: SelectionBuilder[${safeTypeName(t.name)}, $typeLetter]""").mkString(", ")})",
              writeType(field.ofType).replace(fieldType, typeLetter),
              writeTypeBuilder(
                field.ofType,
                s"ChoiceOf(Map(${interfaceTypes.map(t => s""""${t.name}" -> Obj(on${t.name})""").mkString(", ")}))"
              )
            )
          }
        } else {
          (
            s"[$typeLetter]",
            s"(innerSelection: SelectionBuilder[$fieldType, $typeLetter])",
            safeFieldTypeReplace(writeType(field.ofType), fieldType, typeLetter),
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

      val name          =
        if ((optionalUnion && unionTypes.nonEmpty) || (optionalInterface && interfaceTypes.nonEmpty))
          safeName(field.name + "Option")
        else if (commonInterface && interfaceTypes.nonEmpty)
          safeName(field.name + "Interface")
        else safeName(field.name)
      val owner         = if (typeParam.nonEmpty) Some(fieldType) else None
      val fieldTypeInfo = FieldTypeInfo(
        field.name,
        name,
        outputType,
        interfaceTypes.map(_.name),
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

    def writeField(
      field: FieldDefinition,
      typeName: String,
      optionalUnion: Boolean,
      optionalInterface: Boolean,
      commonInterface: Boolean
    ): String =
      writeFieldInfo(collectFieldInfo(field, typeName, optionalUnion, optionalInterface, commonInterface))

    def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
      typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

    def writeRootQueryType(
      typedef: ObjectTypeDefinition
    ): String =
      s"type ${typedef.name} = _root_.caliban.client.Operations.RootQuery"

    def writeRootQuery(typedef: ObjectTypeDefinition): String =
      s"""object ${typedef.name} {
         |  ${typedef.fields.flatMap { field =>
          if (isOptionalInterfaceType(field)) {
            Vector(
              writeField(
                field,
                "_root_.caliban.client.Operations.RootQuery",
                optionalUnion = false,
                optionalInterface = false,
                commonInterface = false
              ),
              writeField(
                field,
                "_root_.caliban.client.Operations.RootQuery",
                optionalUnion = false,
                optionalInterface = true,
                commonInterface = false
              )
            )
          } else {
            Vector(
              writeField(
                field,
                "_root_.caliban.client.Operations.RootQuery",
                optionalUnion = false,
                optionalInterface = false,
                commonInterface = false
              )
            )
          }
        }
          .mkString("\n  ")}
         |}
         |""".stripMargin

    def writeRootMutationType(
      typedef: ObjectTypeDefinition
    ): String =
      s"type ${typedef.name} = _root_.caliban.client.Operations.RootMutation"

    def writeRootMutation(typedef: ObjectTypeDefinition): String =
      s"""object ${typedef.name} {
         |  ${typedef.fields.flatMap { field =>
          if (isOptionalInterfaceType(field)) {
            Vector(
              writeField(
                field,
                "_root_.caliban.client.Operations.RootMutation",
                optionalUnion = false,
                optionalInterface = false,
                commonInterface = false
              ),
              writeField(
                field,
                "_root_.caliban.client.Operations.RootMutation",
                optionalUnion = false,
                optionalInterface = true,
                commonInterface = false
              )
            )
          } else {
            Vector(
              writeField(
                field,
                "_root_.caliban.client.Operations.RootMutation",
                optionalUnion = false,
                optionalInterface = false,
                commonInterface = false
              )
            )
          }
        }
          .mkString("\n  ")}
         |}
         |""".stripMargin

    def writeRootSubscriptionType(
      typedef: ObjectTypeDefinition
    ): String =
      s"type ${typedef.name} = _root_.caliban.client.Operations.RootSubscription"

    def writeRootSubscription(typedef: ObjectTypeDefinition): String =
      s"""object ${typedef.name} {
         |  ${typedef.fields.flatMap { field =>
          if (isOptionalInterfaceType(field)) {
            Vector(
              writeField(
                field,
                "_root_.caliban.client.Operations.RootSubscription",
                optionalUnion = false,
                optionalInterface = false,
                commonInterface = false
              ),
              writeField(
                field,
                "_root_.caliban.client.Operations.RootSubscription",
                optionalUnion = false,
                optionalInterface = true,
                commonInterface = false
              )
            )
          } else {
            Vector(
              writeField(
                field,
                "_root_.caliban.client.Operations.RootSubscription",
                optionalUnion = false,
                optionalInterface = false,
                commonInterface = false
              )
            )
          }
        }
          .mkString("\n  ")}
         |}
         |""".stripMargin

    def writeObjectType(typedef: ObjectTypeDefinition): String = {

      val objectName: String = safeTypeName(typedef.name)

      s"type $objectName"
    }

    def writeObject(typedef: ObjectTypeDefinition, genView: Boolean): String = {

      val objectName: String = safeTypeName(typedef.name)

      val optionalUnionTypeFields = typedef.fields.flatMap { field =>
        if (isOptionalUnionType(field))
          Some(
            collectFieldInfo(
              field,
              objectName,
              optionalUnion = true,
              optionalInterface = false,
              commonInterface = false
            )
          )
        else None
      }

      val optionalInterfaceTypeFields = typedef.fields.flatMap { field =>
        if (isOptionalInterfaceType(field))
          Vector(
            collectFieldInfo(
              field,
              objectName,
              optionalUnion = false,
              optionalInterface = true,
              commonInterface = false
            ),
            collectFieldInfo(
              field,
              objectName,
              optionalUnion = false,
              optionalInterface = false,
              commonInterface = true
            )
          )
        else Vector.empty
      }

      val fields = typedef.fields.map(
        collectFieldInfo(_, objectName, optionalUnion = false, optionalInterface = false, commonInterface = false)
      )
      val view   = if (genView) "\n  " + writeView(typedef.name, fields.map(_.typeInfo)) else ""

      val allFields = fields ++ optionalUnionTypeFields ++ optionalInterfaceTypeFields

      s"""object $objectName {$view
         |  ${allFields.distinct.map(writeFieldInfo).mkString("\n  ")}
         |}
         |""".stripMargin
    }

    def writeView(objectName: String, fields: List[FieldTypeInfo]): String = {
      val viewName       = s"${objectName}View"
      val safeObjectName = safeTypeName(objectName)

      def argumentName(fieldName: String, argName: String): String =
        fieldName + argName.capitalize

      def withRoundBrackets(input: List[String]): String =
        if (input.nonEmpty) input.mkString("(", ", ", ")") else ""

      def withRoundBracketsTuples(input: List[String]): String =
        if (input.nonEmpty) {
          if (input.size > MaxTupleLength)
            input.grouped(MaxTupleLength).map(_.mkString("(", ", ", ")")).mkString("(", ", ", ")")
          else
            input.mkString("(", ", ", ")")
        } else ""

      val genericSelectionFields =
        fields.collect {
          case field if field.owner.nonEmpty =>
            field -> s"${field.rawName}Selection"
        }

      val genericSelectionFieldTypes =
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
            interfaceTypes.map(intType => s"${fieldName}On$intType: SelectionBuilder[$intType, $tpe]")
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

      val viewFunctionBody: String = {
        val selectors = fields.map { case field @ FieldTypeInfo(_, _, _, interfaceTypes, unionTypes, _, _) =>
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
        }
        if (selectors.length > MaxTupleLength) {
          selectors.grouped(MaxTupleLength).map(_.mkString("(", " ~ ", ")")).mkString("(", " ~ ", ")")
        } else selectors.mkString(" ~ ")
      }

      val viewClassFieldParams: String = withRoundBrackets(viewClassFields)

      val viewFunction: String =
        fields match {
          case Nil         => throw new Exception("Invalid GraphQL Schema: an object must have at least one field")
          case head :: Nil =>
            s"$viewFunctionBody.map(${head.name} => $viewName(${head.name}))"

          case other =>
            val unapply = withRoundBracketsTuples(fields.map(field => safeUnapplyName(field.rawName)))
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
    ): String = {
      val inputObjectName = safeTypeName(typedef.name)
      s"""final case class $inputObjectName(${writeArgumentFields(typedef.fields)})
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
    ): String = t match {
      case NamedType(name, true)   =>
        if (name == typeName) s"encode($fieldName)"
        else s"implicitly[ArgEncoder[${safeTypeName(name)}]].encode($fieldName)"
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
    ): String = {

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
        (if (extensibleEnums) Some(s"final case class __Unknown(value: String) extends $enumName") else None)

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

          val values: scala.collection.immutable.Vector[$enumName] = scala.collection.immutable.Vector(${typedef.enumValuesDefinition
          .map(v => safeEnumValue(v.enumValue))
          .mkString(", ")})
        }
       """
    }

    def writeScalar(
      typedef: ScalarTypeDefinition
    ): String =
      s"""type ${safeTypeName(typedef.name)} = String
        """

    @tailrec
    def getTypeLetter(typesMap: Map[String, TypeDefinition], letter: String = "A"): String =
      if (!typesMap.contains(letter)) letter else getTypeLetter(typesMap, letter + "A")

    def writeArgumentFields(
      args: List[InputValueDefinition]
    ): String =
      s"${args.map(arg => s"${safeName(arg.name)} : ${writeType(arg.ofType)}${writeDefaultArgument(arg)}").mkString(", ")}"

    def writeDefaultArgument(arg: InputValueDefinition): String =
      arg.ofType match {
        case t if t.nullable => " = None"
        case ListType(_, _)  => " = Nil"
        case _               => ""
      }

    def writeType(t: Type): String = t match {
      case NamedType(name, true)   => safeTypeName(name)
      case NamedType(name, false)  => s"scala.Option[${safeTypeName(name)}]"
      case ListType(ofType, true)  => s"List[${writeType(ofType)}]"
      case ListType(ofType, false) => s"scala.Option[List[${writeType(ofType)}]]"
    }

    def safeFieldTypeReplace(writtenType: String, fieldType: String, typeLetter: String): String =
      if (fieldType == "Option") {
        writtenType.reverse.replaceFirst(fieldType.reverse, typeLetter).reverse
      } else {
        writtenType.replace(fieldType, typeLetter)
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

    def isScalarSupported(scalar: String): Boolean =
      supportedScalars.contains(scalar) || scalarMappingsWithDefaults.contains(scalar)

    val schemaDef = schema.schemaDefinition

    val interfaceTypes =
      if (splitFiles) schema.interfaceTypeDefinitions.map { typedef =>
        writeObjectType(
          ObjectTypeDefinition(
            description = typedef.description,
            name = typedef.name,
            implements = List.empty,
            directives = typedef.directives,
            fields = typedef.fields
          )
        )
      }
      else Nil

    val interfaces = schema.interfaceTypeDefinitions.map { typedef =>
      val objDef      = ObjectTypeDefinition(
        description = typedef.description,
        name = typedef.name,
        implements = List.empty,
        directives = typedef.directives,
        fields = typedef.fields
      )
      val content     = writeObject(objDef, genView)
      val fullContent =
        if (splitFiles)
          s"""import caliban.client.FieldBuilder._
             |import caliban.client._
             |
             |$content
             |""".stripMargin
        else
          s"""${writeObjectType(objDef)}
             |$content
             |""".stripMargin
      safeTypeName(typedef.name) -> fullContent
    }

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
      .filter(e => !scalarMappingsWithDefaults.contains(e.name))
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
           |  ${(scalars ::: interfaceTypes ::: objectTypes ::: queryTypes ::: mutationTypes ::: subscriptionTypes)
            .mkString("\n")}
           |}
           |""".stripMargin
      val classFiles        =
        (enums ::: interfaces ::: objects ::: inputs ::: queries.toList ::: mutations.toList ::: subscriptions.toList).map {
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
          else ""}${if (
            interfaces.nonEmpty || objects.nonEmpty || queries.nonEmpty || mutations.nonEmpty || subscriptions.nonEmpty
          )
            """import caliban.client.FieldBuilder._
              |""".stripMargin
          else
            ""}${if (
            enums.nonEmpty || interfaces.nonEmpty || objects.nonEmpty || queries.nonEmpty || mutations.nonEmpty || subscriptions.nonEmpty || inputs.nonEmpty
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
                            |  ${interfaces.map(_._2).mkString("\n")}
                            |  ${objects.map(_._2).mkString("\n")}
                            |  ${inputs.map(_._2).mkString("\n")}
                            |  ${queries.map(_._2).mkString("\n")}
                            |  ${mutations.map(_._2).mkString("\n")}
                            |  ${subscriptions.map(_._2).mkString("\n")}
                            |
                            |}""".stripMargin)
    }
  }

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
