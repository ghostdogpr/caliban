package caliban.gateway.internal

import caliban.Value.{ BooleanValue, StringValue }
import caliban.gateway.Lookup
import caliban.introspection.adt._
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
  federation: Boolean,
  lookups: List[Lookup]
)

private[gateway] final class ComposedGraph private[internal] (
  val rootType: RootType,
  private val routes: Map[(OperationType, String), String],
  private val fieldRoutes: Map[(String, String), List[String]],
  private val entityLookups: Map[(String, String), ComposedGraph.EntityLookup]
) {
  def source(operation: OperationType, field: String): Option[String] =
    routes.get(operation -> field)

  def source(typeName: String, field: String, preferred: String): Option[String] =
    fieldRoutes.get(typeName -> field).flatMap { sources =>
      if (sources.contains(preferred)) Some(preferred) else sources.headOption
    }

  def lookup(source: String, typeName: String): Option[ComposedGraph.EntityLookup] =
    entityLookups.get(source -> typeName)
}

private[gateway] object ComposedGraph {
  final case class EntityLookup(keyFields: List[String], operation: LookupOperation)

  sealed trait LookupOperation {
    def requiresTypename: Boolean
  }

  object LookupOperation {
    case object FederationEntities extends LookupOperation {
      val requiresTypename: Boolean = true
    }

    final case class GraphQLQuery(
      field: String,
      arguments: Map[String, LookupArgument],
      result: LookupResult
    ) extends LookupOperation {
      val requiresTypename: Boolean = false
    }
  }

  sealed trait LookupArgument

  object LookupArgument {
    final case class Key(field: String, expectedType: __Type)              extends LookupArgument
    final case class ObjectMapping(fields: List[(String, LookupArgument)]) extends LookupArgument
    final case class Batch(value: LookupArgument)                          extends LookupArgument
  }

  sealed trait LookupResult

  object LookupResult {
    case object Single extends LookupResult

    sealed trait ListResult extends LookupResult

    case object Ordered                                 extends ListResult
    final case class ByKey(fields: Map[String, String]) extends ListResult
  }
}

private[gateway] object SchemaComposition {

  def isFederation(document: Document): Boolean =
    federationLinks(document).nonEmpty || document.objectTypeDefinitions.exists(_.fields.exists(_.name == "_entities"))

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
      (lookupDiagnostics(schemas) :::
        duplicateRootDiagnostics(OperationType.Query, queryFields) :::
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
      val lookups                                      =
        types.flatMap(entry => entry.entity.toList.flatMap(_.lookup).map((entry.source -> entry.name) -> _)).toMap

      SchemaValidator
        .validateRootType(rootType)
        .left
        .map(error => List(s"[composition] ${error.getMessage}"))
        .map(_ => new ComposedGraph(rootType, routes, fieldRoutes, lookups))
    }
  }

  private def lookupDiagnostics(schemas: List[SchemaContribution]): List[String] =
    schemas.flatMap { schema =>
      val sourceKind =
        if (schema.federation && schema.lookups.nonEmpty)
          List(s"[${schema.name}] Ordinary GraphQL lookups cannot be declared on a Federation subgraph.")
        else Nil
      val duplicates = schema.lookups
        .groupBy(_.typeName)
        .collect {
          case (typeName, values) if values.size > 1 =>
            s"[${schema.name}] More than one lookup is declared for type '$typeName'."
        }
        .toList
      sourceKind ::: duplicates ::: schema.lookups.flatMap(validateLookup(schema, _))
    }

  private def validateLookup(schema: SchemaContribution, lookup: Lookup): List[String] = {
    val prefix      = s"[${schema.name}]"
    val targetType  = schema.rootType.types.get(lookup.typeName)
    val rootName    = schema.rootType.queryType.name.getOrElse("Query")
    val sourceField = schema.rootType.queryType.allFields.find(_.name == lookup.field)
    val keyNames    = lookup.keyFields
    val keys        = targetType.toList
      .flatMap(target => keyNames.flatMap(name => target.allFields.find(_.name == name).map(name -> _)))
      .toMap

    val targetDiagnostics      = targetType match {
      case None                                             =>
        List(s"$prefix Lookup target type '${lookup.typeName}' does not exist.")
      case Some(target) if target.kind != __TypeKind.OBJECT =>
        List(s"$prefix Lookup target type '${lookup.typeName}' must be an object type.")
      case Some(_)                                          => Nil
    }
    val keyDiagnostics         =
      (if (keyNames.isEmpty) List(s"$prefix Lookup for '${lookup.typeName}' must declare at least one key field.")
       else Nil) :::
        keyNames
          .groupBy(identity)
          .collect {
            case (name, values) if values.size > 1 =>
              s"$prefix Lookup key field '${lookup.typeName}.$name' is declared more than once."
          }
          .toList :::
        targetType.toList.flatMap { target =>
          keyNames.flatMap { name =>
            target.allFields.find(_.name == name) match {
              case None        => List(s"$prefix Lookup key field '${lookup.typeName}.$name' does not exist.")
              case Some(field) =>
                val kind = nullableType(field._type).kind
                if (kind == __TypeKind.SCALAR || kind == __TypeKind.ENUM) Nil
                else List(s"$prefix Lookup key field '${lookup.typeName}.$name' must be a scalar or enum.")
            }
          }
        }
    val fieldDiagnostics       = sourceField match {
      case None        => List(s"$prefix Lookup field '$rootName.${lookup.field}' does not exist.")
      case Some(field) =>
        val resultType  = nullableType(field._type)
        val shapeValid  = lookup match {
          case _: Lookup.Single     => resultType.kind != __TypeKind.LIST && resultType.name.contains(lookup.typeName)
          case _: Lookup.ListLookup =>
            resultType.kind == __TypeKind.LIST && resultType.ofType
              .map(nullableType)
              .exists(element => element.kind != __TypeKind.LIST && element.name.contains(lookup.typeName))
        }
        val shape       = lookup match {
          case _: Lookup.Single     => s"'${lookup.typeName}'"
          case _: Lookup.ListLookup => s"a list of '${lookup.typeName}'"
        }
        val shapeErrors =
          if (shapeValid) Nil else List(s"$prefix Lookup field '$rootName.${lookup.field}' must return $shape.")
        shapeErrors ::: validateLookupArguments(prefix, rootName, lookup, field, keys)
    }
    val correlationDiagnostics = (lookup, targetType, sourceField) match {
      case (list: Lookup.ListLookup, Some(target), Some(field)) =>
        validateCorrelation(prefix, rootName, list, field, target, keys)
      case _                                                    => Nil
    }

    targetDiagnostics ::: keyDiagnostics ::: fieldDiagnostics ::: correlationDiagnostics
  }

  private def validateLookupArguments(
    prefix: String,
    rootName: String,
    lookup: Lookup,
    field: __Field,
    keys: Map[String, __Field]
  ): List[String] = {
    val arguments   = field.allArgs.map(argument => argument.name -> argument).toMap
    val unknown     = lookup.arguments.keysIterator
      .filterNot(arguments.contains)
      .map(name => s"$prefix Lookup field '$rootName.${lookup.field}' has no argument '$name'.")
      .toList
    val missing     = field.allArgs.collect {
      case argument
          if !lookup.arguments.contains(argument.name) && !argument._type.isNullable && argument.defaultValue.isEmpty =>
        s"$prefix Required lookup argument '${lookup.field}.${argument.name}' has no mapping."
    }
    val mappings    = lookup.arguments.toList.flatMap { case (name, mapping) =>
      arguments.get(name).toList.flatMap(argument => validateArgument(prefix, name, mapping, argument._type, keys))
    }
    val batch       = lookup match {
      case _: Lookup.Single if lookup.arguments.values.exists(containsBatch)                               =>
        List(s"$prefix Single lookup argument mappings cannot contain a batch mapping.")
      case _: Lookup.ListLookup if !lookup.arguments.values.exists(containsBatch)                          =>
        List(s"$prefix List lookup argument mappings must contain a batch mapping.")
      case _: Lookup.ListLookup if lookup.arguments.values.exists(keyOutsideBatch(_, insideBatch = false)) =>
        List(s"$prefix List lookup key mappings must be nested inside a batch mapping.")
      case _                                                                                               => Nil
    }
    val mappedKeys  = lookup.arguments.valuesIterator.flatMap(argumentKeys).toSet
    val keyCoverage =
      if (mappedKeys == lookup.keyFields.toSet) Nil
      else List(s"$prefix Lookup argument mappings must use every declared key field.")

    unknown ::: missing ::: mappings ::: batch ::: keyCoverage
  }

  private def validateArgument(
    prefix: String,
    path: String,
    mapping: Lookup.Argument,
    expected: __Type,
    keys: Map[String, __Field]
  ): List[String] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Key(field)            =>
        keys.get(field) match {
          case None           => List(s"$prefix Lookup argument '$path' references undeclared key field '$field'.")
          case Some(keyField) =>
            if (compatibleValueType(keyField._type, valueType)) Nil
            else
              List(
                s"$prefix Lookup argument '$path' is incompatible with key field '${keyField.name}'."
              )
        }
      case Lookup.Argument.ObjectMapping(fields) =>
        if (valueType.kind != __TypeKind.INPUT_OBJECT)
          List(s"$prefix Lookup argument '$path' maps an object into a non-input-object value.")
        else {
          val inputFields = valueType.allInputFields.map(field => field.name -> field).toMap
          val duplicates  = fields
            .groupBy(_._1)
            .collect {
              case (name, values) if values.size > 1 =>
                s"$prefix Lookup argument '$path.$name' is mapped more than once."
            }
            .toList
          val unknown     = fields.collect {
            case (name, _) if !inputFields.contains(name) =>
              s"$prefix Lookup input field '$path.$name' does not exist."
          }
          val names       = fields.iterator.map(_._1).toSet
          val missing     = valueType.allInputFields.collect {
            case input if !names.contains(input.name) && !input._type.isNullable && input.defaultValue.isEmpty =>
              s"$prefix Required lookup input field '$path.${input.name}' has no mapping."
          }
          duplicates ::: unknown ::: missing ::: fields.flatMap { case (name, value) =>
            inputFields
              .get(name)
              .toList
              .flatMap(input => validateArgument(prefix, s"$path.$name", value, input._type, keys))
          }
        }
      case Lookup.Argument.Batch(value)          =>
        if (containsBatch(value)) List(s"$prefix Lookup argument '$path' cannot nest a batch mapping.")
        else if (!valueType.isList) List(s"$prefix Lookup argument '$path' maps a batch into a non-list value.")
        else validateArgument(prefix, path, value, valueType.ofType.map(nullableType).getOrElse(valueType), keys)
    }
  }

  private def validateCorrelation(
    prefix: String,
    rootName: String,
    lookup: Lookup.ListLookup,
    field: __Field,
    target: __Type,
    keys: Map[String, __Field]
  ): List[String] =
    lookup.correlation match {
      case Lookup.Correlation.Ordered       => Nil
      case Lookup.Correlation.ByKey(fields) =>
        val nullability = nullableType(field._type).ofType match {
          case Some(element) if !element.isNullable => Nil
          case _                                    =>
            List(s"$prefix By-key lookup field '$rootName.${lookup.field}' must return non-null items.")
        }
        val coverage    =
          if (fields.values.toList.sorted == lookup.keyFields.sorted) Nil
          else List(s"$prefix By-key lookup correlation must map every declared key field exactly once.")
        val values      = fields.toList.flatMap { case (responseField, keyField) =>
          target.allFields.find(_.name == responseField) match {
            case None                =>
              List(s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' does not exist.")
            case Some(responseValue) =>
              keys.get(keyField) match {
                case None           => List(s"$prefix Lookup correlation references undeclared key field '$keyField'.")
                case Some(keyValue) =>
                  if (compatibleValueType(responseValue._type, keyValue._type)) Nil
                  else
                    List(
                      s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' is incompatible with key '$keyField'."
                    )
              }
          }
        }
        nullability ::: coverage ::: values
    }

  private def containsBatch(argument: Lookup.Argument): Boolean =
    argument match {
      case _: Lookup.Argument.Key                => false
      case Lookup.Argument.ObjectMapping(fields) => fields.exists(value => containsBatch(value._2))
      case _: Lookup.Argument.Batch              => true
    }

  private def keyOutsideBatch(argument: Lookup.Argument, insideBatch: Boolean): Boolean =
    argument match {
      case _: Lookup.Argument.Key                => !insideBatch
      case Lookup.Argument.ObjectMapping(fields) => fields.exists(value => keyOutsideBatch(value._2, insideBatch))
      case Lookup.Argument.Batch(value)          => keyOutsideBatch(value, insideBatch = true)
    }

  private def argumentKeys(argument: Lookup.Argument): List[String] =
    argument match {
      case Lookup.Argument.Key(field)            => field :: Nil
      case Lookup.Argument.ObjectMapping(fields) => fields.flatMap(value => argumentKeys(value._2))
      case Lookup.Argument.Batch(value)          => argumentKeys(value)
    }

  private def nullableType(tpe: __Type): __Type =
    if (tpe.kind == __TypeKind.NON_NULL) tpe.ofType.map(nullableType).getOrElse(tpe) else tpe

  private def compatibleValueType(left: __Type, right: __Type): Boolean = {
    val a = nullableType(left)
    val b = nullableType(right)
    a.kind == b.kind && a.name == b.name
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
    entity: Option[EntityDefinition],
    ownedFields: Set[String],
    hiddenDirectives: Set[String]
  )

  private final case class EntityDefinition(
    keyFields: List[String],
    operation: Option[ComposedGraph.LookupOperation]
  ) {
    def lookup: Option[ComposedGraph.EntityLookup] =
      operation.map(ComposedGraph.EntityLookup(keyFields, _))
  }

  private final case class FederationKey(fields: List[String], resolvable: Boolean)

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
    val entity      =
      if (schema.federation)
        directives.collectFirst(Function.unlift(keyDirective(_, names))).map { key =>
          val operation =
            if (key.resolvable && hasEntityLookup(schema, name))
              Some(ComposedGraph.LookupOperation.FederationEntities)
            else None
          EntityDefinition(key.fields, operation)
        }
      else
        schema.lookups
          .find(_.typeName == name)
          .map { lookup =>
            EntityDefinition(lookup.keyFields, compileLookup(schema, lookup))
          }
    val external    = fields.collect {
      case field
          if schema.federation && field.directives.exists(directive => names.external.contains(directive.name)) =>
        field.name
    }.toSet
    TypeEntry(
      schema.name,
      name,
      tpe,
      entity,
      tpe.allFields.map(_.name).toSet -- external,
      if (schema.federation) names.hidden else Set.empty
    )
  }

  private def compileLookup(
    schema: SchemaContribution,
    lookup: Lookup
  ): Option[ComposedGraph.LookupOperation.GraphQLQuery] =
    schema.rootType.queryType.allFields.find(_.name == lookup.field).flatMap { field =>
      val argumentTypes = field.allArgs.map(argument => argument.name -> argument._type).toMap
      val arguments     = lookup.arguments.toList.foldLeft(Option(List.empty[(String, ComposedGraph.LookupArgument)])) {
        case (compiled, (name, mapping)) =>
          for {
            values   <- compiled
            expected <- argumentTypes.get(name)
            value    <- compileArgument(mapping, expected)
          } yield (name -> value) :: values
      }
      val result        = lookup match {
        case _: Lookup.Single         => ComposedGraph.LookupResult.Single
        case value: Lookup.ListLookup =>
          value.correlation match {
            case Lookup.Correlation.Ordered       => ComposedGraph.LookupResult.Ordered
            case Lookup.Correlation.ByKey(fields) => ComposedGraph.LookupResult.ByKey(fields)
          }
      }
      arguments.map(values => ComposedGraph.LookupOperation.GraphQLQuery(lookup.field, values.reverse.toMap, result))
    }

  private def compileArgument(
    mapping: Lookup.Argument,
    expected: __Type
  ): Option[ComposedGraph.LookupArgument] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Key(field)            =>
        Some(ComposedGraph.LookupArgument.Key(field, valueType))
      case Lookup.Argument.ObjectMapping(fields) =>
        val inputFields = valueType.allInputFields.map(field => field.name -> field._type).toMap
        fields
          .foldLeft(Option(List.empty[(String, ComposedGraph.LookupArgument)])) { case (compiled, (name, value)) =>
            for {
              values    <- compiled
              inputType <- inputFields.get(name)
              nested    <- compileArgument(value, inputType)
            } yield (name -> nested) :: values
          }
          .map(values => ComposedGraph.LookupArgument.ObjectMapping(values.reverse))
      case Lookup.Argument.Batch(value)          =>
        valueType.ofType
          .map(nullableType)
          .flatMap(compileArgument(value, _))
          .map(ComposedGraph.LookupArgument.Batch.apply)
    }
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
      if (kinds.size > 1) List(s"[type $name] Entity kinds are incompatible between subgraphs.") else Nil
    val fieldDiagnostics = fields.toList.flatMap { case (fieldName, definitions) =>
      val values = definitions.map(_._2)
      val owned  = values.filter { case (entry, _) => entry.ownedFields.contains(fieldName) }
      val key    = entries.exists(_.entity.exists(_.keyFields.contains(fieldName)))
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
    entries.exists(_.entity.nonEmpty)

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
    val links                = federationLinks(document)
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

  private def federationLinks(document: Document): List[Directive] = {
    val schemaDirectives = document.schemaDefinition.toList.flatMap(_.directives) :::
      document.typeExtensions.collect { case extension: SchemaExtension => extension }.flatMap(_.directives)

    schemaDirectives.filter(directive =>
      directive.name == "link" && directive.arguments.get("url").exists {
        case StringValue(url) => url.startsWith("https://specs.apollo.dev/federation/")
        case _                => false
      }
    )
  }

  private def keyDirective(
    directive: Directive,
    names: FederationDirectiveNames
  ): Option[FederationKey] =
    if (!names.key.contains(directive.name)) None
    else
      directive.arguments.get("fields").collect {
        case StringValue(value) if value.trim.matches("[_A-Za-z][_0-9A-Za-z]*") =>
          val resolvable = !directive.arguments.get("resolvable").contains(BooleanValue(false))
          FederationKey(value.trim :: Nil, resolvable)
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
