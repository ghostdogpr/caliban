package caliban.gateway.internal

import caliban.introspection.adt.{ __Directive, __Field, __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.{ Directive, Document, OperationType }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.validation.SchemaValidator

import scala.collection.immutable.ListMap

private[gateway] final case class SchemaContribution(name: String, rootType: RootType)

private[gateway] final class ComposedGraph private[internal] (
  val rootType: RootType,
  private val routes: Map[(OperationType, String), String]
) {
  def source(operation: OperationType, field: String): Option[String] =
    routes.get(operation -> field)
}

private[gateway] object SchemaComposition {

  def compose(contributions: List[SchemaContribution]): Either[List[String], ComposedGraph] = {
    val schemas     = contributions.sortBy(_.name)
    val queryFields = rootFields(schemas, OperationType.Query)
    val mutations   = rootFields(schemas, OperationType.Mutation)
    val types       = nonRootTypes(schemas)
    val directives  = schemas.flatMap(schema => schema.rootType.additionalDirectives.map(schema.name -> _))
    val diagnostics =
      (duplicateRootDiagnostics(OperationType.Query, queryFields) :::
        duplicateRootDiagnostics(OperationType.Mutation, mutations) :::
        incompatibleTypeDiagnostics(types) :::
        incompatibleDirectiveDiagnostics(directives)).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else {
      val query                                        = makeRootType("Query", queryFields.map(_._2))
      val mutation                                     = if (mutations.nonEmpty) Some(makeRootType("Mutation", mutations.map(_._2))) else None
      val additional                                   = chooseCompatible(types)
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

      SchemaValidator
        .validateRootType(rootType)
        .left
        .map(error => List(s"[composition] ${error.getMessage}"))
        .map(_ => new ComposedGraph(rootType, routes))
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
      root.toList.flatMap(_.allFields.map(schema.name -> _))
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

  private def nonRootTypes(schemas: List[SchemaContribution]): List[(String, __Type)] =
    schemas.flatMap { schema =>
      val rootNames =
        schema.rootType.queryType.name.toSet ++
          schema.rootType.mutationType.flatMap(_.name).toSet ++
          schema.rootType.subscriptionType.flatMap(_.name).toSet

      schema.rootType.types.valuesIterator
        .filterNot(tpe => tpe.name.exists(rootNames.contains))
        .map(schema.name -> _)
        .toList
    }

  private def incompatibleTypeDiagnostics(types: List[(String, __Type)]): List[String] =
    types.flatMap { case (source, tpe) => tpe.name.map(name => name -> (source -> typeSignature(tpe))) }
      .groupBy(_._1)
      .collect {
        case (name, definitions) if definitions.map(_._2._2).distinct.size > 1 =>
          val sources = definitions.map(_._2._1).distinct.sorted.map(source => s"'$source'").mkString(", ")
          s"[type $name] Definitions are incompatible between subgraphs: $sources."
      }
      .toList

  private def incompatibleDirectiveDiagnostics(directives: List[(String, __Directive)]): List[String] =
    directives
      .groupBy(_._2.name)
      .collect {
        case (name, definitions) if definitions.map(entry => directiveSignature(entry._2)).distinct.size > 1 =>
          val sources = definitions.map(_._1).distinct.sorted.map(source => s"'$source'").mkString(", ")
          s"[directive @$name] Definitions are incompatible between subgraphs: $sources."
      }
      .toList

  private def chooseCompatible(types: List[(String, __Type)]): List[__Type] =
    types.flatMap { case (source, tpe) => tpe.name.map(name => name -> (source -> tpe)) }
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .flatMap { case (_, definitions) => definitions.map(_._2).sortBy(_._1).headOption.map(_._2) }

  private def chooseCompatibleDirectives(directives: List[(String, __Directive)]): List[__Directive] =
    directives
      .groupBy(_._2.name)
      .toList
      .sortBy(_._1)
      .flatMap { case (_, definitions) => definitions.sortBy(_._1).headOption.map(_._2) }

  private def makeRootType(name: String, fields: List[__Field]): __Type = {
    val sorted = fields.sortBy(_.name)
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
}
