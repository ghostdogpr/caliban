package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.CompositionDiagnostic.{ error, Code }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition._
import caliban.gateway.internal.composition.FederationCompilation.FederationApplication
import caliban.gateway.internal.composition.FederationCompilation.FederationDirective._
import caliban.gateway.internal.composition.FieldSelectionMap.SelectionMapUse
import caliban.introspection.adt._
import caliban.parsing.adt.Directive

import scala.collection.compat._

/**
 * Compiles the `@require` arguments of a composite source schema into the requirements the gateway fills.
 */
private[composition] object RequireCompilation {

  /**
   * Requirements by client field, and the `@require` selection maps to validate once every subgraph is merged.
   */
  final case class Compiled(
    diagnostics: List[CompositionDiagnostic],
    requirements: Map[FieldCoordinate, List[RequireArgument]],
    maps: List[SelectionMapUse]
  )

  def compile(subgraph: Source): Compiled = {
    val (errors, compiled) = subgraph
      .applications(Require)
      .collect { case FederationApplication(at: ArgumentCoordinate, _, directive) =>
        compileArgument(subgraph, at, directive)
      }
      .flatten
      .partitionMap(identity)
    Compiled(
      errors.flatten ::: externalCollisions(subgraph, compiled.map(_._1.at)) ::: inconsistentImplementations(subgraph),
      compiled.groupMap(entry => FieldCoordinate(entry._1.at.typeName, entry._1.at.fieldName))(_._1),
      compiled.map(_._2)
    )
  }

  private def compileArgument(
    subgraph: Source,
    at: ArgumentCoordinate,
    directive: Directive
  ): Option[Either[List[CompositionDiagnostic], (RequireArgument, SelectionMapUse)]] = {
    val coordinate                           = SchemaCoordinate.Argument(at.typeName, at.fieldName, at.argumentName)
    def invalid(code: Code)(message: String) = List(error(code, List(subgraph.name), Some(coordinate))(message))
    for {
      field    <- subgraph.sourceField(at.typeName, at.fieldName)
      argument <- field.allArgs.find(_.name == at.argumentName)
      value    <- stringArgument(directive.arguments, "field")
    } yield for {
      _      <- Either.cond(
                  !subgraph.applied(LookupField, FieldCoordinate(at.typeName, at.fieldName)),
                  (),
                  invalid(Code.RequireInvalidUsage)(
                    s"@require on '${coordinate.render}' cannot be applied to an argument of a @lookup field."
                  )
                )
      parsed <- FieldSelectionMap
                  .parse(value)
                  .left
                  .map(message =>
                    invalid(Code.RequireInvalidSyntax)(
                      s"Invalid @require field selection map on '${coordinate.render}': $message"
                    )
                  )
    } yield {
      val mapping  = subgraph.mapping
      val selected = mapping.clientSelectionMap(parsed, Some(mapping.sourceType(at.typeName)))
      RequireArgument(at, selected, argument._type) ->
        SelectionMapUse(Require, coordinate, selected, argument._type, at.typeName)
    }
  }

  private def externalCollisions(subgraph: Source, required: List[ArgumentCoordinate]): List[CompositionDiagnostic] =
    required.map(at => FieldCoordinate(at.typeName, at.fieldName)).distinct.collect {
      case at @ FieldCoordinate(typeName, fieldName)
          if subgraph.applied(External, at) || subgraph.rootType.types
            .get(typeName)
            .exists(tpe => subgraph.applied(External, TypeCoordinate(typeName, typeLocation(tpe.kind)))) =>
        val field = SchemaCoordinate.Member(typeName, fieldName)
        error(Code.ExternalRequireCollision, List(subgraph.name), Some(field))(
          s"External field '${field.render}' cannot declare @require arguments."
        )
    }

  private def inconsistentImplementations(subgraph: Source): List[CompositionDiagnostic] = {
    def required(typeName: String, fieldName: String, argument: String): Boolean =
      subgraph.applied(Require, ArgumentCoordinate(typeName, fieldName, argument))

    for {
      (typeName, tpe) <- subgraph.rootType.types.toList
      interfaceName   <- tpe.interfaces().getOrElse(Nil).flatMap(_.name)
      interface       <- subgraph.rootType.types.get(interfaceName).toList
      field           <- interface.allFields
      implementation  <- fieldDefinition(tpe, field.name).toList
      argument        <- field.allArgs
      if implementation.allArgs.exists(_.name == argument.name) &&
        required(interfaceName, field.name, argument.name) != required(typeName, field.name, argument.name)
    } yield {
      val at       = SchemaCoordinate.Argument(typeName, field.name, argument.name)
      val declared = SchemaCoordinate.Argument(interfaceName, field.name, argument.name)
      error(Code.RequireInconsistentOnImplementation, List(subgraph.name), Some(at))(
        s"Argument '${at.render}' must apply @require exactly when '${declared.render}' does."
      )
    }
  }
}
