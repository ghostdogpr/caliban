package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition.{ ArgumentCoordinate, FieldCoordinate, TypeCoordinate }
import caliban.gateway.internal.composition.FederationCompilation._
import caliban.gateway.internal.composition.SchemaComposer.PreparedSubgraph
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Selection }
import caliban.rendering.DocumentRenderer
import caliban.schema.Types

import scala.collection.compat._

private[composition] object ContextCompilation {
  def compile(subgraph: PreparedSubgraph): Either[List[String], FederationContexts] = {
    val contexts       = subgraph.applications(FederationDirective.Context).collect {
      case FederationApplication(TypeCoordinate(typeName, _), _, directive) =>
        ContextDeclaration(typeName, ContextName(stringArgument(directive.arguments, "name").getOrElse("")))
    }
    val typesByContext = contexts.groupMap(_.name)(_.typeName)

    def compileContextArgument(
      at: ArgumentCoordinate,
      parent: __Type,
      argument: __InputValue,
      directive: Directive
    ): Either[String, (FieldCoordinate, ContextArgument)] = {
      val result = for {
        value             <- stringArgument(directive.arguments, "field").toRight("the 'field' argument must be a string.")
        parsed            <- parseSelection(value).toRight("the context selection could not be parsed.")
        (name, selections) = parsed
        contextTypes      <- typesByContext.get(name).toRight(s"context '${name.value}' is not declared by this subgraph.")
        _                 <- Either.cond(argument._type.isNullable, (), "context arguments must be nullable.")
        _                 <- Either.cond(argument.defaultValue.isEmpty, (), "context arguments must not define a default value.")
        _                 <- validateContextReceiver(subgraph, parent, at.typeName, at.fieldName)
        _                 <- validateSelections(subgraph, selections, topLevel = true)
        parents           <- contextParents(subgraph, contextTypes)
        _                 <- validateContextTypeConditions(parents, selections)
        // The last-declared context type is checked first, so its error is the one reported.
        _                 <- traverseEither(parents.reverse)(validateContextValue(subgraph, _, selections, argument._type))
      } yield FieldCoordinate(at.typeName, at.fieldName) -> ContextArgument(argument.name, name, selections)
      result.left.map(error =>
        s"[${subgraph.name}] Invalid Federation @fromContext application at '${at.display}': $error"
      )
    }

    val bindings = subgraph.applications(FederationDirective.FromContext).flatMap {
      case FederationApplication(at @ ArgumentCoordinate(typeName, fieldName, argumentName), _, directive) =>
        for {
          parent   <- subgraph.rootType.types.get(typeName)
          field    <- fieldDefinition(parent, fieldName)
          argument <- field.allArgs.find(_.name == argumentName)
        } yield compileContextArgument(at, parent, argument, directive)
      case _                                                                                               => None
    }

    validated(contexts.flatMap(declarationDiagnostics(subgraph.name, _)), validateAll(bindings)).map { bound =>
      FederationContexts(contexts.distinct, bound.groupMap(_._1)(_._2))
    }
  }

  private def validateContextReceiver(
    subgraph: PreparedSubgraph,
    parent: __Type,
    typeName: String,
    fieldName: String
  ): Either[String, Unit] = {
    val implementsField = parent.interfaces().getOrElse(Nil).exists(fieldDefinition(_, fieldName).nonEmpty)
    val isEntityObject  = subgraph.entityLookups(typeName).nonEmpty && parent.kind == __TypeKind.OBJECT &&
      !subgraph.isInterfaceObject(typeName)
    if (implementsField) Left("context arguments cannot be used on fields that implement interface fields.")
    else if (!isEntityObject) Left("the containing object must define a resolvable entity lookup.")
    else Right(())
  }

  private def validateSelections(
    subgraph: PreparedSubgraph,
    values: List[Selection],
    topLevel: Boolean
  ): Either[String, Unit] =
    traverseEither(values) {
      case Selection.Field(alias, _, _, directives, children, _)         =>
        if (alias.nonEmpty) Left("aliases are not allowed in a context selection.")
        else if (directives.nonEmpty) Left("directives are not allowed in a context selection.")
        else validateSelections(subgraph, children, topLevel = false)
      case Selection.InlineFragment(typeCondition, directives, children) =>
        val condition = typeCondition.flatMap(value => subgraph.rootType.types.get(value.name))
        if (directives.nonEmpty) Left("directives are not allowed in a context selection.")
        else if (!topLevel) Left("inline fragments are only allowed at the top level of a context selection.")
        else if (!condition.exists(_.kind == __TypeKind.OBJECT))
          Left("top-level context type conditions must name concrete object types.")
        else if (condition.exists(_.name.exists(subgraph.isInterfaceObject)))
          Left(InterfaceObjectContextSelection)
        else validateSelections(subgraph, children, topLevel = false)
      case _: Selection.FragmentSpread                                   =>
        Left("fragment spreads are not allowed in a context selection.")
    }.map(_ => ())

  private def contextParents(subgraph: PreparedSubgraph, contextTypes: List[String]): Either[String, List[__Type]] =
    traverseEither(contextTypes) { contextType =>
      if (subgraph.isInterfaceObject(contextType))
        Left(s"context type '$contextType' cannot be an @interfaceObject.")
      else subgraph.rootType.types.get(contextType).toRight(s"context type '$contextType' does not exist.")
    }

  private def validateContextValue(
    subgraph: PreparedSubgraph,
    parent: __Type,
    selections: List[Selection],
    argumentType: __Type
  ): Either[String, Unit] =
    contextSelectionTypes(subgraph, parent, selections).flatMap { values =>
      Either.cond(
        values.forall(compatibleValueType(_, argumentType)),
        (),
        s"the selected value is incompatible with argument type '${DocumentRenderer.renderTypeName(argumentType)}'."
      )
    }

  private def contextSelectionTypes(
    subgraph: PreparedSubgraph,
    parent: __Type,
    selections: List[Selection]
  ): Either[String, List[__Type]] = {
    val rootType = subgraph.rootType

    def projectType(container: __Type, selected: __Type): __Type =
      nullableType(container) match {
        case list if list.kind == __TypeKind.LIST => list.copy(ofType = list.ofType.map(projectType(_, selected)))
        case _                                    => selected
      }

    def applies(fragment: Selection.InlineFragment, runtime: __Type): Boolean =
      fragment.typeCondition.forall(condition => runtime.name.contains(condition.name))

    def resolveField(staticType: __Type, field: Selection.Field): Either[String, List[__Type]] =
      if (field.name == TypenameField && field.selectionSet.isEmpty) Right(Types.string :: Nil)
      else
        fieldDefinition(staticType, field.name)
          .toRight(s"field '${field.name}' does not exist on context type '${staticType.name.getOrElse("")}'.")
          .flatMap { definition =>
            val output = definition._type.innerType
            if (field.selectionSet.isEmpty) Right(definition._type :: Nil)
            else if (output.name.exists(subgraph.isInterfaceObject)) Left(InterfaceObjectContextSelection)
            else
              traverseEither(contextPossibleTypes(output))(resolve(output, _, field.selectionSet))
                .map(_.flatten.map(projectType(definition._type, _)))
          }

    def resolve(staticType: __Type, runtime: __Type, values: List[Selection]): Either[String, List[__Type]] = {
      val selected = values.filter {
        case fragment: Selection.InlineFragment => applies(fragment, runtime)
        case _                                  => true
      }
      selected match {
        case Nil                                         => Left("the context selection does not match this context type.")
        case (field: Selection.Field) :: Nil             => resolveField(staticType, field)
        case (fragment: Selection.InlineFragment) :: Nil =>
          val narrowed =
            fragment.typeCondition.flatMap(condition => rootType.types.get(condition.name)).getOrElse(staticType)
          resolve(narrowed, runtime, fragment.selectionSet)
        case _                                           => Left("the context selection resolves to multiple fields.")
      }
    }

    traverseEither(contextPossibleTypes(parent))(resolve(parent, _, selections)).map(_.flatten)
  }

  final case class FederationContexts(
    declarations: List[ContextDeclaration],
    contextBindings: Map[FieldCoordinate, List[ContextArgument]]
  )

  def parseSelection(value: String): Option[(ContextName, List[Selection])] =
    value match {
      case ContextSelectionPattern(name, selections) if isContextName(name) =>
        val parsed =
          if (selections.startsWith("{")) parseSelectionSet(s"query $selections")
          else parseFieldSet(selections)
        parsed.map(ContextName(name) -> _)
      case _                                                                => None
    }

  private val ContextSelectionPattern         =
    raw"(?:[\n\r\t ,]|#[^\n\r]*+)*+\$$(?:[\n\r\t ,]|#[^\n\r]*+)*+([A-Za-z_]\w*)(?:[\n\r\t ,]|#[^\n\r]*+)*+([\s\S]*)".r
  private val ContextNamePattern              = raw"[A-Za-z][A-Za-z0-9]*".r
  private val InterfaceObjectContextSelection = "context selections cannot reference an @interfaceObject type."

  private def isContextName(name: String): Boolean = ContextNamePattern.pattern.matcher(name).matches()

  private def declarationDiagnostics(source: String, declaration: ContextDeclaration): List[String] = {
    val ContextDeclaration(typeName, ContextName(name)) = declaration
    if (!isContextName(name)) List(s"[$source] Invalid Federation @context name '$name' on '$typeName'.")
    else if (typeName == "Mutation" || typeName == "Subscription")
      List(s"[$source] Federation @context is not supported on the $typeName root type '$typeName'.")
    else Nil
  }

  // validateSelections has already checked that each top-level type condition names an object type.
  private def validateContextTypeConditions(
    locations: List[__Type],
    selections: List[Selection]
  ): Either[String, Unit] = {
    val locationTypes = locations.iterator.flatMap(contextPossibleTypes).flatMap(_.name).toSet
    val unused        = selections.collect {
      case Selection.InlineFragment(Some(condition), _, _) if !locationTypes(condition.name) => condition.name
    }.distinct.sorted
    Either.cond(
      unused.isEmpty,
      (),
      s"top-level context type conditions do not match a context location: ${unused.mkString(", ")}."
    )
  }

  private def contextPossibleTypes(tpe: __Type): List[__Type] =
    if (isAbstractType(tpe)) tpe.possibleTypes.getOrElse(Nil) else tpe :: Nil
}
