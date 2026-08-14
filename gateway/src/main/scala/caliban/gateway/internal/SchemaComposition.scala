package caliban.gateway.internal

import caliban.Value.{ BooleanValue, StringValue }
import caliban.introspection.adt.{ __Directive, __Field, __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension.ObjectTypeExtension
import caliban.parsing.adt.{ Directive, Document, OperationType }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.validation.SchemaValidator

import scala.collection.immutable.ListMap

private[gateway] final case class SchemaContribution(
  name: String,
  rootType: RootType,
  document: Document,
  federation: Boolean
)

private[gateway] final class ComposedGraph private[internal] (
  val rootType: RootType,
  private val routes: Map[(OperationType, String), String],
  private val fieldRoutes: Map[(String, String), List[String]],
  private val entityKeys: Map[(String, String), ComposedGraph.EntityKey],
  private val entityLookups: Set[(String, String)]
) {
  def source(operation: OperationType, field: String): Option[String] =
    routes.get(operation -> field)

  def source(typeName: String, field: String, preferred: String): Option[String] =
    fieldRoutes.get(typeName -> field).flatMap { sources =>
      if (sources.contains(preferred)) Some(preferred) else sources.headOption
    }

  def key(source: String, typeName: String): Option[ComposedGraph.EntityKey] =
    entityKeys.get(source -> typeName)

  def canLookup(source: String, typeName: String): Boolean =
    entityLookups.contains(source -> typeName)
}

private[gateway] object ComposedGraph {
  final case class EntityKey(field: String, resolvable: Boolean)
}

private[gateway] object SchemaComposition {

  def compose(contributions: List[SchemaContribution]): Either[List[String], ComposedGraph] = {
    val schemas     = contributions.sortBy(_.name)
    val queryFields = rootFields(schemas, OperationType.Query)
    val mutations   = rootFields(schemas, OperationType.Mutation)
    val types       = nonRootTypes(schemas)
    val directives  = schemas.flatMap(schema =>
      schema.rootType.additionalDirectives
        .filterNot(directive =>
          schema.federation && federationDirectiveNames(schema.document).hidden.contains(directive.name)
        )
        .map(schema.name -> _)
    )
    val diagnostics =
      (duplicateRootDiagnostics(OperationType.Query, queryFields) :::
        duplicateRootDiagnostics(OperationType.Mutation, mutations) :::
        incompatibleTypeDiagnostics(types) :::
        incompatibleDirectiveDiagnostics(directives)).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else {
      lazy val additionalByName: Map[String, __Type]   = chooseCompatible(types, additionalByName)
      def rewrite(tpe: __Type): __Type                 = rewriteType(tpe, additionalByName)
      val query                                        = makeRootType("Query", queryFields.map(_._2), rewrite)
      val mutation                                     =
        if (mutations.nonEmpty) Some(makeRootType("Mutation", mutations.map(_._2), rewrite)) else None
      val additional                                   = additionalByName.toList.sortBy(_._1).map(_._2)
      val rootType                                     = RootType(
        query,
        mutation,
        None,
        additional,
        chooseCompatibleDirectives(directives)
      )
      val routes: Map[(OperationType, String), String] = (queryFields.map { case (source, field) =>
        (OperationType.Query -> field.name) -> source
      } ::: mutations.map { case (source, field) =>
        (OperationType.Mutation -> field.name) -> source
      }).toMap
      val fieldRoutes                                  = types
        .flatMap(entry => entry.ownedFields.map(field => (entry.name -> field) -> entry.source))
        .groupBy(_._1)
        .map { case (coordinate, providers) => coordinate -> providers.map(_._2).distinct.sorted }
      val entityKeys                                   = types.flatMap(entry => entry.key.map(key => (entry.source -> entry.name) -> key)).toMap
      val lookups                                      = types.collect {
        case entry
            if entry.key.exists(_.resolvable) && schemas
              .find(_.name == entry.source)
              .exists(hasEntityLookup(_, entry.name)) =>
          entry.source -> entry.name
      }.toSet

      SchemaValidator
        .validateRootType(rootType)
        .left
        .map(error => List(s"[composition] ${error.getMessage}"))
        .map(_ => new ComposedGraph(rootType, routes, fieldRoutes, entityKeys, lookups))
    }
  }

  private def rootFields(
    schemas: List[SchemaContribution],
    operation: OperationType
  ): List[(String, __Field)] =
    schemas.flatMap { schema =>
      val root = operation match {
        case OperationType.Query        => Some(schema.rootType.queryType)
        case OperationType.Mutation     => schema.rootType.mutationType
        case OperationType.Subscription => None
      }
      root.toList.flatMap(
        _.allFields
          .filterNot(field => schema.federation && isTransportField(field.name))
          .map(schema.name -> _)
      )
    }.sortBy { case (source, field) => field.name -> source }

  private def duplicateRootDiagnostics(
    operation: OperationType,
    fields: List[(String, __Field)]
  ): List[String] =
    fields
      .groupBy(_._2.name)
      .collect {
        case (field, providers) if providers.size > 1 =>
          val sources = providers.map(_._1).distinct.sorted.map(name => s"'$name'").mkString(", ")
          s"[${operation.toString.toLowerCase}.$field] Root field is owned by multiple subgraphs: $sources."
      }
      .toList

  private final case class TypeEntry(
    source: String,
    name: String,
    tpe: __Type,
    federation: Boolean,
    key: Option[ComposedGraph.EntityKey],
    ownedFields: Set[String],
    hiddenDirectives: Set[String]
  )

  private def nonRootTypes(schemas: List[SchemaContribution]): List[TypeEntry] =
    schemas.flatMap { schema =>
      val rootNames =
        schema.rootType.queryType.name.toSet ++
          schema.rootType.mutationType.flatMap(_.name).toSet ++
          schema.rootType.subscriptionType.flatMap(_.name).toSet

      schema.rootType.types.valuesIterator
        .filterNot(tpe =>
          tpe.name.exists(name =>
            rootNames.contains(name) ||
              schema.federation && federationDirectiveNames(schema.document).hiddenTypes.contains(name)
          )
        )
        .flatMap(tpe => tpe.name.map(name => typeEntry(schema, name, tpe)))
        .toList
    }

  private def typeEntry(schema: SchemaContribution, name: String, tpe: __Type): TypeEntry = {
    val definitions = schema.document.objectTypeDefinitions.filter(_.name == name)
    val extensions  = schema.document.typeExtensions.collect {
      case extension: ObjectTypeExtension if extension.name == name => extension
    }
    val directives  = definitions.flatMap(_.directives) ::: extensions.flatMap(_.directives)
    val fields      = definitions.flatMap(_.fields) ::: extensions.flatMap(_.fields)
    val names       = federationDirectiveNames(schema.document)
    val key         = if (schema.federation) directives.collectFirst(Function.unlift(keyDirective(_, names))) else None
    val external    = fields.collect {
      case field
          if schema.federation && field.directives.exists(directive => names.external.contains(directive.name)) =>
        field.name
    }.toSet
    TypeEntry(
      schema.name,
      name,
      tpe,
      schema.federation,
      key,
      tpe.allFields.map(_.name).toSet -- external,
      if (schema.federation) names.hidden else Set.empty
    )
  }

  private def incompatibleTypeDiagnostics(types: List[TypeEntry]): List[String] =
    types
      .groupBy(_.name)
      .toList
      .flatMap { case (name, entries) =>
        if (isEntity(entries)) incompatibleEntityDiagnostics(name, entries)
        else if (entries.map(entry => typeSignature(entry.tpe)).distinct.size > 1) {
          val sources = entries.map(_.source).distinct.sorted.map(source => s"'$source'").mkString(", ")
          List(s"[type $name] Definitions are incompatible between subgraphs: $sources.")
        } else Nil
      }

  private def incompatibleEntityDiagnostics(name: String, entries: List[TypeEntry]): List[String] = {
    val kinds  = entries.map(_.tpe.kind).distinct
    val fields =
      entries.flatMap(entry => entry.tpe.allFields.map(field => field.name -> (entry -> field))).groupBy(_._1)

    val kindDiagnostic   =
      if (kinds.size > 1) List(s"[type $name] Federation entity kinds are incompatible between subgraphs.") else Nil
    val fieldDiagnostics = fields.toList.flatMap { case (fieldName, definitions) =>
      val values = definitions.map(_._2)
      val owned  = values.filter { case (entry, _) => entry.ownedFields.contains(fieldName) }
      val key    = entries.exists(_.key.exists(_.field == fieldName))
      if (values.map { case (_, field) => fieldSignature(field) }.distinct.size > 1 || owned.size > 1 && !key) {
        val sources = values.map(_._1.source).distinct.sorted.map(source => s"'$source'").mkString(", ")
        List(s"[type $name.$fieldName] Definitions are incompatible between subgraphs: $sources.")
      } else Nil
    }
    kindDiagnostic ::: fieldDiagnostics
  }

  private def incompatibleDirectiveDiagnostics(directives: List[(String, __Directive)]): List[String] =
    directives
      .groupBy(_._2.name)
      .collect {
        case (name, definitions) if definitions.map(entry => directiveSignature(entry._2)).distinct.size > 1 =>
          val sources = definitions.map(_._1).distinct.sorted.map(source => s"'$source'").mkString(", ")
          s"[directive @$name] Definitions are incompatible between subgraphs: $sources."
      }
      .toList

  private def chooseCompatible(types: List[TypeEntry], all: => Map[String, __Type]): Map[String, __Type] =
    types
      .groupBy(_.name)
      .map { case (name, entries) =>
        val sorted = entries.sortBy(_.source)
        val chosen =
          if (isEntity(sorted)) mergeEntity(sorted, rewriteType(_, all))
          else sanitizeType(sorted.head.tpe, rewriteType(_, all), sorted.head.hiddenDirectives)
        name -> chosen
      }

  private def isEntity(entries: List[TypeEntry]): Boolean =
    entries.exists(entry => entry.federation && entry.key.nonEmpty)

  private def mergeEntity(entries: List[TypeEntry], rewrite: __Type => __Type): __Type = {
    val base             = entries.head.tpe
    val hiddenDirectives = entries.iterator.flatMap(_.hiddenDirectives).toSet
    val fields           = entries
      .flatMap(entry => entry.tpe.allFields.map(field => field.name -> (entry.source -> field)))
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .flatMap { case (_, definitions) => definitions.map(_._2).sortBy(_._1).headOption.map(_._2) }
      .map(sanitizeField(_, rewrite, hiddenDirectives))

    sanitizeType(base, rewrite, hiddenDirectives).copy(fields = _ => Some(fields))
  }

  private def sanitizeType(tpe: __Type, rewrite: __Type => __Type, hiddenDirectives: Set[String]): __Type =
    tpe.copy(
      directives = filterFederationDirectives(tpe.directives, hiddenDirectives),
      fields = args => tpe.fields(args).map(_.map(sanitizeField(_, rewrite, hiddenDirectives)))
    )

  private def sanitizeField(field: __Field, rewrite: __Type => __Type, hiddenDirectives: Set[String]): __Field =
    field.copy(
      `type` = () => rewrite(field.`type`()),
      directives = filterFederationDirectives(field.directives, hiddenDirectives)
    )

  private def filterFederationDirectives(
    directives: Option[List[Directive]],
    hiddenDirectives: Set[String]
  ): Option[List[Directive]] =
    if (hiddenDirectives.isEmpty) directives
    else directives.map(_.filterNot(directive => hiddenDirectives.contains(directive.name))).filter(_.nonEmpty)

  private def rewriteType(tpe: __Type, types: => Map[String, __Type]): __Type =
    tpe.ofType match {
      case Some(ofType) => tpe.copy(ofType = Some(rewriteType(ofType, types)))
      case None         => tpe.name.flatMap(types.get).getOrElse(tpe)
    }

  private def chooseCompatibleDirectives(directives: List[(String, __Directive)]): List[__Directive] =
    directives
      .groupBy(_._2.name)
      .toList
      .sortBy(_._1)
      .flatMap { case (_, definitions) => definitions.sortBy(_._1).headOption.map(_._2) }

  private def makeRootType(name: String, fields: List[__Field], rewrite: __Type => __Type): __Type = {
    val sorted = fields.sortBy(_.name).map(field => field.copy(`type` = () => rewrite(field.`type`())))
    __Type(
      kind = __TypeKind.OBJECT,
      name = Some(name),
      fields = args => Some(if (args.includeDeprecated.getOrElse(false)) sorted else sorted.filterNot(_.isDeprecated))
    )
  }

  private def typeSignature(tpe: __Type): String =
    tpe.toTypeDefinition.fold(tpe.kind.toString)(definition => render(normalize(definition)))

  private def directiveSignature(directive: __Directive): String =
    render(
      directive.toDirectiveDefinition.copy(
        description = None,
        args = directive.toDirectiveDefinition.args.map(normalize).sortBy(_.name)
      )
    )

  private def normalize(definition: TypeDefinition): TypeDefinition =
    definition match {
      case ObjectTypeDefinition(_, name, interfaces, directives, fields)    =>
        ObjectTypeDefinition(
          None,
          name,
          interfaces.sortBy(_.name),
          normalizeDirectives(directives),
          fields.map(normalize).sortBy(_.name)
        )
      case InterfaceTypeDefinition(_, name, interfaces, directives, fields) =>
        InterfaceTypeDefinition(
          None,
          name,
          interfaces.sortBy(_.name),
          normalizeDirectives(directives),
          fields.map(normalize).sortBy(_.name)
        )
      case InputObjectTypeDefinition(_, name, directives, fields)           =>
        InputObjectTypeDefinition(None, name, normalizeDirectives(directives), fields.map(normalize).sortBy(_.name))
      case EnumTypeDefinition(_, name, directives, values)                  =>
        EnumTypeDefinition(
          None,
          name,
          normalizeDirectives(directives),
          values
            .map(value => value.copy(description = None, directives = normalizeDirectives(value.directives)))
            .sortBy(
              _.enumValue
            )
        )
      case UnionTypeDefinition(_, name, directives, members)                =>
        UnionTypeDefinition(None, name, normalizeDirectives(directives), members.sorted)
      case ScalarTypeDefinition(_, name, directives)                        =>
        ScalarTypeDefinition(None, name, normalizeDirectives(directives))
    }

  private def normalize(field: FieldDefinition): FieldDefinition =
    field.copy(
      description = None,
      args = field.args.map(normalize).sortBy(_.name),
      directives = normalizeDirectives(field.directives)
    )

  private def normalize(input: InputValueDefinition): InputValueDefinition =
    input.copy(description = None, directives = normalizeDirectives(input.directives))

  private def normalizeDirectives(directives: List[Directive]): List[Directive] =
    directives
      .map(directive => directive.copy(arguments = ListMap(directive.arguments.toList.sortBy(_._1): _*)))
      .sortBy(directive =>
        directive.name -> directive.arguments.iterator.map { case (name, value) =>
          s"$name=${value.toInputString}"
        }.mkString
      )

  private def render(definition: caliban.parsing.adt.Definition): String =
    DocumentRenderer.renderCompact(Document(definition :: Nil, SourceMapper.empty))

  private final case class FederationDirectiveNames(
    key: Set[String],
    external: Set[String],
    hidden: Set[String],
    hiddenTypes: Set[String]
  )

  private final case class ImportedName(name: String, alias: String, directive: Boolean)

  private def federationDirectiveNames(document: Document): FederationDirectiveNames = {
    val schemaDirectives     = document.schemaDefinition.toList.flatMap(_.directives) :::
      document.typeExtensions.collect { case extension: SchemaExtension => extension }.flatMap(_.directives)
    val links                = schemaDirectives.filter(directive =>
      directive.name == "link" && directive.arguments.get("url").exists {
        case StringValue(url) => url.startsWith("https://specs.apollo.dev/federation/")
        case _                => false
      }
    )
    val imported             = links.flatMap(_.arguments.get("import").toList).flatMap {
      case caliban.InputValue.ListValue(values) => values
      case _                                    => Nil
    }
    val imports              = imported.flatMap {
      case StringValue(name)                      =>
        Some(ImportedName(name.stripPrefix("@"), name.stripPrefix("@"), name.startsWith("@")))
      case caliban.InputValue.ObjectValue(fields) =>
        fields.get("name").collect { case StringValue(name) => name }.map { name =>
          val alias = fields.get("as").collect { case StringValue(value) => value }.getOrElse(name)
          ImportedName(name.stripPrefix("@"), alias.stripPrefix("@"), name.startsWith("@"))
        }
      case _                                      => None
    }
    val aliases              = imports.iterator.map(value => value.name -> value.alias).toMap
    val federationNamespaces = links.iterator
      .map(
        _.arguments
          .get("as")
          .collect { case StringValue(value) => value.stripPrefix("@") }
          .getOrElse("federation")
      )
      .toSet
    val namespaces           = federationNamespaces ++ (if (links.nonEmpty) Set("link") else Set.empty)
    val namespacePrefix      = namespaces.map(_ + "__")
    val hiddenTypes          = document.typeDefinitions.iterator
      .map(_.name)
      .filter(name => namespacePrefix.exists(name.startsWith))
      .toSet ++ imports.collect { case value if !value.directive => value.alias } ++
      Set("_Any", "_Entity", "_FieldSet", "_Service")
    val hiddenDirectives     = Set("link", "key", "external", "extends") ++
      imports.collect { case value if value.directive => value.alias } ++
      document.directiveDefinitions.iterator.map(_.name).filter(name => namespacePrefix.exists(name.startsWith))

    FederationDirectiveNames(
      Set("key", "federation__key") ++ aliases.get("key") ++ federationNamespaces.map(_ + "__key"),
      Set("external", "federation__external") ++ aliases.get("external") ++
        federationNamespaces.map(_ + "__external"),
      hiddenDirectives,
      hiddenTypes
    )
  }

  private def keyDirective(
    directive: Directive,
    names: FederationDirectiveNames
  ): Option[ComposedGraph.EntityKey] =
    if (!names.key.contains(directive.name)) None
    else
      directive.arguments.get("fields").collect {
        case StringValue(value) if value.trim.matches("[_A-Za-z][_0-9A-Za-z]*") =>
          val resolvable = !directive.arguments.get("resolvable").contains(BooleanValue(false))
          ComposedGraph.EntityKey(value.trim, resolvable)
      }

  private def fieldSignature(field: __Field): String =
    render(
      ObjectTypeDefinition(
        None,
        "Signature",
        Nil,
        Nil,
        List(field.toFieldDefinition.copy(description = None, directives = Nil))
      )
    )

  private def hasEntityLookup(schema: SchemaContribution, entityType: String): Boolean =
    schema.rootType.queryType.allFields.find(_.name == "_entities") match {
      case None        => schema.federation
      case Some(field) =>
        val acceptsRepresentations = field.allArgs.find(_.name == "representations").exists { argument =>
          argument._type.isList && argument._type.innerType.name.contains("_Any")
        }
        val returnsEntities        = field._type.isList && field._type.innerType.name.contains("_Entity")
        val includesEntity         = field._type.innerType.possibleTypes.exists(_.exists(_.name.contains(entityType)))
        acceptsRepresentations && returnsEntities && includesEntity
    }

  private def isTransportField(name: String): Boolean =
    name == "_entities" || name == "_service"

}
