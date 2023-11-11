package caliban.tools

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, Type }

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

    def writeRootQueryOrMutationDef(op: ObjectTypeDefinition): String = {
      val typeParamOrEmpty = if (isEffectTypeAbstract) s"[$effect[_]]" else ""
      s"""
         |${writeDescription(op.description)}final case class ${op.name}$typeParamOrEmpty(
         |${op.fields.map(c => writeRootField(c, op)).mkString(",\n")}
         |)$derivesSchema""".stripMargin

    }
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
      s"""${writeDescription(typedef.description)}final case class ${typedef.name}(${typedef.fields
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

    def writeField(field: FieldDefinition, of: TypeDefinition, isMethod: Boolean): String =
      if (field.args.nonEmpty) {
        s"${writeDescription(field.description)}${if (isMethod) "def " else ""}${safeName(field.name)} : ${argsName(field, of)} => ${writeMaybeEffectType(field)}"
      } else {
        s"""${writeDescription(field.description)}${if (isMethod) "def " else ""}${safeName(
          field.name
        )} : ${writeMaybeEffectType(field)}"""
      }

    def writeInputValue(value: InputValueDefinition): String =
      s"""${writeDescription(value.description)}${safeName(value.name)} : ${writeType(value.ofType)}"""

    def writeArguments(field: FieldDefinition, of: TypeDefinition): String = {
      def fields(args: List[InputValueDefinition]): String =
        s"${args.map(arg => s"${safeName(arg.name)} : ${writeType(arg.ofType)}").mkString(", ")}"

      if (field.args.nonEmpty) {
        s"final case class ${argsName(field, of)}(${fields(field.args)})$derivesSchemaAndArgBuilder"
      } else {
        ""
      }
    }

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

    def writeMaybeEffectType(field: FieldDefinition): String =
      if (field.directives.exists(d => d.name == "lazy"))
        writeEffectType(field.ofType)
      else writeType(field.ofType)

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

    s"""${packageName.fold("")(p => s"package $p\n\n")}${if (hasTypes && hasOperations) "import Types._\n" else ""}
          ${if (typesAndOperations.contains("@GQL")) "import caliban.schema.Annotations._\n" else ""}
          ${if (hasSubscriptions) "import zio.stream.ZStream\n" else ""}
          $additionalImportsString

      $typesAndOperations
      """
  }
}
