package caliban.tools

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Directives.{ LazyDirective, NewtypeDirective }
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Directive, Directives, Document, Type }

import scala.annotation.tailrec
import scala.collection.compat._

object SchemaWriter {

  def write(
    schema: Document,
    packageName: Option[String] = None,
    effect: String = "zio.UIO",
    imports: Option[List[String]] = None,
    scalarMappings: Option[Map[String, String]],
    isEffectTypeAbstract: Boolean = false,
    preserveInputNames: Boolean = false,
    addDerives: Boolean = false
  ): String = {
    val derivesSchema: String =
      if (addDerives) " derives caliban.schema.Schema.SemiAuto" else ""

    val derivesSchemaAndArgBuilder: String =
      if (addDerives) " derives caliban.schema.Schema.SemiAuto, caliban.schema.ArgBuilder" else ""

    val interfaceImplementationsMap: Map[InterfaceTypeDefinition, List[ObjectTypeDefinition]] = (for {
      objectDef    <- schema.objectTypeDefinitions
      interfaceDef <- schema.interfaceTypeDefinitions
      if objectDef.implements.exists(_.name == interfaceDef.name)
    } yield interfaceDef -> objectDef).groupBy(_._1).map { case (definition, tuples) =>
      definition -> tuples.map(_._2)
    }

    val interfacesExtendedForObject: Map[ObjectTypeDefinition, List[InterfaceTypeDefinition]] =
      interfaceImplementationsMap.iterator.flatMap { case (i, os) => os.map(o => (o, i)) }.toList.groupMap {
        case (o, _) => o
      } { case (_, i) => i }

    lazy val typeNameToDefinitionMap: Map[String, TypeDefinition] =
      schema.typeDefinitions.map(obj => obj.name -> obj).toMap
    lazy val typeNameToNestedFields                               = typeNameToDefinitionMap.map { case (name, t) =>
      name -> findNestedFieldTypes(t, typeNameToDefinitionMap).flatMap(typeNameToDefinitionMap.get)
    }

    def inheritedFromInterface(obj: ObjectTypeDefinition, field: FieldDefinition): Option[InterfaceTypeDefinition] =
      interfacesExtendedForObject.get(obj) flatMap { interfaces =>
        interfaces.find(_.fields.exists(_.name == field.name))
      }

    def reservedType(typeDefinition: ObjectTypeDefinition): Boolean =
      typeDefinition.name == "Query" || typeDefinition.name == "Mutation" || typeDefinition.name == "Subscription"

    def writeRootField(field: FieldDefinition, od: ObjectTypeDefinition): String = {
      val argsTypeName = if (field.args.nonEmpty) s" ${argsName(field, od)} =>" else ""
      s"${safeName(field.name)} :$argsTypeName ${writeEffectType(field.ofType)}"
    }

    def isAbstractEffectful(typedef: ObjectTypeDefinition): Boolean =
      isEffectTypeAbstract && isEffectful(typedef)

    def isEffectful(typedef: ObjectTypeDefinition): Boolean = isLocalEffectful(typedef) || isNestedEffectful(typedef)

    def isLocalEffectful(typedef: ObjectTypeDefinition): Boolean =
      hasFieldWithDirective(typedef, LazyDirective)

    def isNestedEffectful(typedef: ObjectTypeDefinition): Boolean =
      typeNameToNestedFields
        .getOrElse(typedef.name, List.empty)
        .exists(t => hasFieldWithDirective(t, LazyDirective))

    def generic(op: ObjectTypeDefinition, isRootDefinition: Boolean = false): String =
      if ((isRootDefinition && isEffectTypeAbstract) || isAbstractEffectful(op))
        s"[${effect}[_]]"
      else
        s""

    def writeRootQueryOrMutationDef(op: ObjectTypeDefinition): String =
      s"""
         |${writeDescription(op.description)}final case class ${op.name}${generic(op, isRootDefinition = true)}(
         |${op.fields.map(c => writeRootField(c, op)).mkString(",\n")}
         |)$derivesSchema""".stripMargin

    def writeSubscriptionField(field: FieldDefinition, od: ObjectTypeDefinition): String =
      "%s:%s ZStream[Any, Nothing, %s]".format(
        safeName(field.name),
        if (field.args.nonEmpty) s" ${argsName(field, od)} =>" else "",
        writeType(field.ofType)
      )

    def writeRootSubscriptionDef(op: ObjectTypeDefinition): String =
      s"""
         |${writeDescription(op.description)}final case class ${op.name}(
         |${op.fields.map(c => writeSubscriptionField(c, op)).mkString(",\n")}
         |)$derivesSchema""".stripMargin

    def writeObject(typedef: ObjectTypeDefinition, extend: List[String]): String = {
      val extendRendered = extend match {
        case Nil      => ""
        case nonEmpty => s" extends ${nonEmpty.mkString(" with ")}"
      }
      s"""${writeDescription(typedef.description)}final case class ${typedef.name}${generic(typedef)}(${typedef.fields
        .map(field => writeField(field, inheritedFromInterface(typedef, field).getOrElse(typedef), isMethod = false))
        .mkString(", ")})$extendRendered$derivesSchema"""
    }

    def writeInputObject(typedef: InputObjectTypeDefinition): String = {
      val name            = typedef.name
      val maybeAnnotation = if (preserveInputNames) s"""@GQLInputName("$name")\n""" else ""
      s"""$maybeAnnotation${writeDescription(typedef.description)}final case class $name(${typedef.fields
        .map(writeInputValue)
        .mkString(", ")})$derivesSchemaAndArgBuilder"""
    }

    def writeEnum(typedef: EnumTypeDefinition): String =
      s"""${writeDescription(typedef.description)}sealed trait ${typedef.name} extends scala.Product with scala.Serializable$derivesSchemaAndArgBuilder

          object ${typedef.name} {
            ${typedef.enumValuesDefinition
        .map(v =>
          s"${writeDescription(v.description)}case object ${safeName(v.enumValue)} extends ${typedef.name}$derivesSchemaAndArgBuilder"
        )
        .mkString("\n")}
          }
       """

    def writeUnions(unions: List[UnionTypeDefinition]): String =
      unions.map(x => writeUnionSealedTrait(x)).mkString("\n")

    def writeUnionSealedTrait(union: UnionTypeDefinition): String =
      s"""${writeDescription(
        union.description
      )}sealed trait ${union.name} extends scala.Product with scala.Serializable$derivesSchema"""

    def writeInterface(interface: InterfaceTypeDefinition): String =
      s"""@GQLInterface
        ${writeDescription(interface.description)}sealed trait ${interface.name} extends scala.Product with scala.Serializable $derivesSchema {
         ${interface.fields.map(field => writeField(field, interface, isMethod = true)).mkString("\n")}
        }
       """

    def writeField(inputField: FieldDefinition, of: TypeDefinition, isMethod: Boolean): String = {
      def containsNestedDirective(
        field: FieldDefinition,
        directive: String
      ): Boolean =
        typeNameToDefinitionMap.get(Type.innerType(field.ofType)).fold(false) { t =>
          hasFieldWithDirective(t, directive)
        } || typeNameToNestedFields
          .getOrElse(Type.innerType(field.ofType), List.empty)
          .exists(t => hasFieldWithDirective(t, directive))

      val field = resolveNewTypeFieldDef(inputField).getOrElse(inputField)

      val fieldIsEffectWrapped               = field.directives.exists(_.name == LazyDirective)
      val fieldTypeIsEffectTypeParameterized = isEffectTypeAbstract && containsNestedDirective(field, LazyDirective)
      val fieldType                          = (fieldIsEffectWrapped, fieldTypeIsEffectTypeParameterized) match {
        case (true, true)   => s"$effect[${writeParameterizedType(field.ofType)}]"
        case (true, false)  => s"$effect[${writeType(field.ofType)}]"
        case (false, true)  => writeParameterizedType(field.ofType)
        case (false, false) => writeType(field.ofType)
      }

      val GQLNewTypeDirective = writeGQLNewTypeDirective(field.directives)

      if (field.args.nonEmpty) {
        s"""$GQLNewTypeDirective${writeDescription(field.description)}${if (isMethod) "def " else ""}${safeName(
          field.name
        )} : ${argsName(field, of)} => $fieldType"""
      } else {
        s"""$GQLNewTypeDirective${writeDescription(field.description)}${if (isMethod) "def " else ""}${safeName(
          field.name
        )} : $fieldType"""
      }
    }

    def writeGQLNewTypeDirective(directives: List[Directive]) =
      directives
        .find(_.name == NewtypeDirective)
        .fold("") { directive =>
          val fnName = directive.arguments("name").toInputString.replace("\"", "")
          s"""@GQLDirective(Directive("$NewtypeDirective", Map("name" -> StringValue("${safeName(fnName)}"))))\n"""
        }

    def writeInputValue(value: InputValueDefinition): String = {
      val GQLNewTypeInputDirective = writeGQLNewTypeDirective(value.directives)

      val inputDef = resolveNewTypeInputDef(value).getOrElse(value)

      s"""$GQLNewTypeInputDirective${writeDescription(inputDef.description)}${safeName(inputDef.name)} : ${writeType(
        inputDef.ofType
      )}"""
    }

    def writeArguments(field: FieldDefinition, of: TypeDefinition): String = {
      def fields(args: List[InputValueDefinition]): String =
        s"${args.map { arg =>
          val resolvedArg = resolveNewTypeInputDef(arg).getOrElse(arg)
          s"${safeName(resolvedArg.name)} : ${writeType(resolvedArg.ofType)}"
        }.mkString(", ")}"

      if (field.args.nonEmpty) {
        s"final case class ${argsName(field, of)}(${fields(field.args)})$derivesSchemaAndArgBuilder"
      } else {
        ""
      }
    }

    def writeNewTypeClasses(fieldType: Type, directive: Directive) = {
      @tailrec def writeTypeOf(fieldType: Type): String = fieldType match {
        case NamedType(name, _) => scalarMappings.flatMap(_.get(name)).getOrElse(name)
        case ListType(ftype, _) => writeTypeOf(ftype)
      }

      val fnName  = safeName(directive.arguments("name").toInputString.replace("\"", ""))
      val newtype = safeName(writeTypeOf(fieldType))

      s"""case class $fnName(value : $newtype) extends AnyVal
         |object $fnName {
         |    implicit val schema: Schema[Any, $fnName] = implicitly[Schema[Any, $newtype]].contramap(_.value)
         |    implicit val argBuilder: ArgBuilder[$fnName] = implicitly[ArgBuilder[$newtype]].map($fnName(_))
         |}""".stripMargin
    }

    def replaceNameOfInnertype(name: String, ftype: Type): Type =
      ftype match {
        case NamedType(_, nt)  => NamedType(name, nt)
        case ListType(nt, opt) => ListType(replaceNameOfInnertype(name, nt), opt)
      }

    def resolveNewTypeFieldDef(field: FieldDefinition): Option[FieldDefinition] =
      if (Directives.isNewType(field.directives)) {
        Directives
          .newTypeName(field.directives)
          .map(name => field.copy(ofType = replaceNameOfInnertype(name, field.ofType)))
      } else None

    def resolveNewTypeInputDef(field: InputValueDefinition): Option[InputValueDefinition] =
      if (Directives.isNewType(field.directives)) {
        Directives
          .newTypeName(field.directives)
          .map(name => field.copy(ofType = replaceNameOfInnertype(name, field.ofType)))
      } else None

    def argsName(field: FieldDefinition, od: TypeDefinition): String =
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

    def writeEffectType(t: Type) =
      s"$effect[${writeType(t)}]"

    def writeType(t: Type): String = {
      def write(name: String): String = scalarMappings
        .flatMap(_.get(name))
        .getOrElse(name)

      t match {
        case NamedType(name, true)   => write(name)
        case NamedType(name, false)  => s"scala.Option[${write(name)}]"
        case ListType(ofType, true)  => s"List[${writeType(ofType)}]"
        case ListType(ofType, false) => s"scala.Option[List[${writeType(ofType)}]]"
      }
    }

    def writeParameterizedType(t: Type): String = {
      def write(name: String): String = {
        val result = scalarMappings
          .flatMap(_.get(name))
          .getOrElse(name)

        s"$result[$effect]"
      }

      t match {
        case Type.NamedType(name, true)   => write(name)
        case Type.NamedType(name, false)  => s"scala.Option[${write(name)}]"
        case Type.ListType(ofType, true)  =>
          s"List[${writeParameterizedType(ofType)}]"
        case Type.ListType(ofType, false) =>
          s"scala.Option[List[${writeParameterizedType(ofType)}]]"
      }
    }

    val schemaDef = schema.schemaDefinition

    val argsTypes = {
      val fromObjects: List[(FieldDefinition, TypeDefinition)]    =
        schema.objectTypeDefinitions.flatMap { typeDef =>
          typeDef.fields.collect {
            case f if f.args.nonEmpty && inheritedFromInterface(typeDef, f).isEmpty => (f, typeDef)
          }
        }
      val fromInterfaces: List[(FieldDefinition, TypeDefinition)] =
        schema.interfaceTypeDefinitions.flatMap(typeDef =>
          typeDef.fields.collect { case f if f.args.nonEmpty => (f, typeDef) }
        )

      (fromObjects ++ fromInterfaces).map { case (field, typeDef) => writeArguments(field, typeDef) }
        .mkString("\n")
    }

    val newTypeClasses = {
      case class FieldAndDirective(fieldName: String, fieldType: Type, directive: Directive)
      val fromObjects                             =
        schema.objectTypeDefinitions.flatMap {
          _.fields.collect {
            case f if f.directives.exists(_.name == NewtypeDirective)                => // FIELD DEFINITION
              List(FieldAndDirective(f.name, f.ofType, f.directives.filter(_.name == NewtypeDirective).head))
            case f if f.args.exists(_.directives.exists(_.name == NewtypeDirective)) => // ARGUMENT DEFINITION
              f.args.collect {
                case a if a.directives.exists(_.name == NewtypeDirective) =>
                  FieldAndDirective(a.name, a.ofType, a.directives.filter(_.name == NewtypeDirective).head)
              }
          }
        }.flatten
      val fromInputTypes: List[FieldAndDirective] =
        schema.inputObjectTypeDefinitions.flatMap {
          _.fields.collect {
            case f if f.directives.exists(_.name == NewtypeDirective) =>
              FieldAndDirective(f.name, f.ofType, f.directives.filter(_.name == NewtypeDirective).head)
          }
        }

      val newtypeClasses = (fromObjects ++ fromInputTypes)
        .groupBy(_.directive.arguments("name").toInputString)
        .map(_._2.head)
        .toList
        .sortBy(_.directive.arguments("name").toInputString)
        .map(fieldAndDirective => writeNewTypeClasses(fieldAndDirective.fieldType, fieldAndDirective.directive))
        .mkString("\n")
      if (newtypeClasses.nonEmpty) newtypeClasses + "\n" else ""
    }

    val unionTypes = schema.unionTypeDefinitions
      .map(union => (union, union.memberTypes.flatMap(schema.objectTypeDefinition)))
      .toMap

    val unions = writeUnions(schema.unionTypeDefinitions)

    val interfacesStr = schema.interfaceTypeDefinitions.map { interface =>
      writeInterface(interface)
    }.mkString("\n")

    val objects = schema.objectTypeDefinitions
      .filterNot(obj =>
        reservedType(obj) ||
          schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
          schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
          schemaDef.exists(_.subscription.getOrElse("Subscription") == obj.name)
      )
      .map { obj =>
        val extendsInterfaces = obj.implements.map(name => name.name)
        val partOfUnionTypes  = unionTypes.collect { case (u, os) if os.exists(_.name == obj.name) => u.name }
        writeObject(obj, extend = extendsInterfaces ++ partOfUnionTypes)
      }
      .mkString("\n")

    val inputs = schema.inputObjectTypeDefinitions.map(writeInputObject).mkString("\n")

    val enums = schema.enumTypeDefinitions.map(writeEnum).mkString("\n")

    val queries = schema
      .objectTypeDefinition(schemaDef.flatMap(_.query).getOrElse("Query"))
      .map(t => writeRootQueryOrMutationDef(t))
      .getOrElse("")

    val mutations = schema
      .objectTypeDefinition(schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
      .map(t => writeRootQueryOrMutationDef(t))
      .getOrElse("")

    val subscriptions = schema
      .objectTypeDefinition(schemaDef.flatMap(_.subscription).getOrElse("Subscription"))
      .map(t => writeRootSubscriptionDef(t))
      .getOrElse("")

    val additionalImportsString = imports.fold("")(_.map(i => s"import $i").mkString("\n"))

    val hasSubscriptions = subscriptions.nonEmpty
    val hasTypes         = argsTypes.length + objects.length + enums.length + unions.length +
      inputs.length + interfacesStr.length > 0
    val hasOperations    = queries.length + mutations.length + subscriptions.length > 0

    val typesAndOperations = s"""
      ${if (hasTypes)
      "object Types {\n" +
        argsTypes + "\n" +
        newTypeClasses +
        objects + "\n" +
        inputs + "\n" +
        unions + "\n" +
        interfacesStr + "\n" +
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

    s"""${packageName.fold("")(p => s"package $p\n\n")}
          ${if (hasTypes && hasOperations) "import Types._\n" else ""}
          ${if (typesAndOperations.contains("@GQL") || newTypeClasses.nonEmpty)
      "import caliban.schema.Annotations._\n"
    else ""}
          ${if (newTypeClasses.nonEmpty)
      """|import caliban.Value._
         |import caliban.parsing.adt.Directive
         |import caliban.schema.{ArgBuilder, Schema}""".stripMargin
    else ""}
          ${if (hasSubscriptions) "import zio.stream.ZStream\n" else ""}
          $additionalImportsString

      $typesAndOperations
      """
  }

  /* Get types for all subfields of an object
    object A {
      field b: B
      field c: String
    }

    object B {
      field d: Int
    }

    result: Set(B, String, Int)
   */
  private def findNestedFieldTypes(
    definition: TypeDefinition,
    typeNameToDefinitionMap: Map[String, TypeDefinition]
  ): Set[String] = {
    def findSubFieldTypes(
      obj: TypeDefinition,
      typeNameToNestedFields: Map[String, Set[String]]
    ): (Set[String], Map[String, Set[String]]) =
      typeNameToNestedFields
        .get(obj.name)
        .fold {
          val fieldTypes: Set[String] = obj match {
            case objectTypeDefinition: ObjectTypeDefinition                      =>
              objectTypeDefinition.fields.map(f => Type.innerType(f.ofType)).toSet
            case interfaceTypeDefinition: TypeDefinition.InterfaceTypeDefinition =>
              interfaceTypeDefinition.fields.map(f => Type.innerType(f.ofType)).toSet
            case inputObjectTypeDefinition: InputObjectTypeDefinition            =>
              inputObjectTypeDefinition.fields.map(f => Type.innerType(f.ofType)).toSet
            case unionTypeDefinition: TypeDefinition.UnionTypeDefinition         =>
              unionTypeDefinition.memberTypes.toSet
            case _                                                               => Set.empty
          }
          val (subTypes, f)           = fieldTypes.foldLeft((Set.empty[String], typeNameToNestedFields)) {
            case ((subTypeSet, reference), t) =>
              typeNameToDefinitionMap.get(t) match {
                case Some(o) =>
                  val (s, f) = findSubFieldTypes(o, reference.updated(obj.name, fieldTypes))
                  (subTypeSet ++ s, f)
                case None    => (subTypeSet, reference)
              }
          }

          val allTypes = fieldTypes ++ subTypes
          (allTypes, f.updated(obj.name, allTypes))

        } { s =>
          (s, typeNameToNestedFields)
        }

    val (s, _) = findSubFieldTypes(definition, Map.empty)
    s
  }

  private def hasFieldWithDirective(definition: TypeDefinition, directive: String): Boolean =
    definition match {
      case ot: ObjectTypeDefinition       =>
        ot.fields.exists(_.directives.exists(_.name == directive))
      case it: InterfaceTypeDefinition    =>
        it.fields.exists(_.directives.exists(_.name == directive))
      case iot: InputObjectTypeDefinition =>
        iot.fields.exists(_.directives.exists(_.name == directive))
      case _: TypeDefinition              => false
    }
}
