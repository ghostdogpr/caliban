package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.FederationCompilation._
import caliban.gateway.internal.composition.SchemaComposer.PreparedSubgraph
import caliban.gateway.internal.composition.TypeComposition.SubgraphType
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Selection }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, InputValueDefinition }
import caliban.rendering.DocumentRenderer
import caliban.schema.{ RootType, Types }
import caliban.Value.StringValue

import scala.annotation.tailrec
import scala.collection.compat._

private[composition] final class ContextCompilation(
  types: List[SubgraphType],
  isInterfaceObject: (String, String) => Boolean
) {
  import ContextCompilation._

  def compile(
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames,
    schema: SchemaInspection
  ): Either[List[String], FederationContexts] =
    if (!subgraph.federation || !names.supportsContexts) Right(FederationContexts(Nil, Nil))
    else {
      val contextDirectives = schema.contextTypes.flatMap { tpe =>
        tpe.directives.collect { case directive if names.context.contains(directive.name) => tpe.name -> directive }
      }
      val typesByContext    = contextDirectives.flatMap { case (typeName, directive) =>
        directive.arguments.get("name").collect { case StringValue(name) => ContextName(name) -> typeName }
      }.groupMap(_._1)(_._2)

      def compileContextArgument(
        typeName: String,
        field: FieldDefinition,
        argument: InputValueDefinition,
        directive: Directive
      ): Either[String, (SourceField, ContextArgument)] = {
        val parentName = subgraph.rootNames.composed(typeName)
        val coordinate = s"$parentName.${field.name}(${argument.name}:)"
        val result     = for {
          value             <- directive.arguments
                                 .get("field")
                                 .collect { case StringValue(value) => value }
                                 .toRight("the 'field' argument must be a string.")
          parsed            <- parseSelection(value).toRight("the context selection could not be parsed.")
          (name, selections) = parsed
          contextTypes       = typesByContext.getOrElse(name, Nil)
          argumentType      <- subgraph.rootType.types
                                 .get(typeName)
                                 .flatMap(tpe => Option(tpe.getFieldOrNull(field.name)))
                                 .flatMap(_.allArgs.find(_.name == argument.name))
                                 .map(_._type)
                                 .toRight("the context argument does not exist in the source schema.")
          _                 <- Either.cond(contextTypes.nonEmpty, (), s"context '${name.value}' is not declared by this subgraph.")
          _                 <- Either.cond(argument.ofType.nullable, (), "context arguments must be nullable.")
          _                 <- Either.cond(argument.defaultValue.isEmpty, (), "context arguments must not define a default value.")
          _                 <- validateContextReceiver(subgraph.name, parentName, field.name)
          _                 <- validateContextSelectionSyntax(selections)
          parents           <- contextParents(subgraph, contextTypes)
          _                 <- validateContextTypeConditions(subgraph.rootType, parents, selections)
          // The last-declared context type is checked first, so its error is the one reported.
          _                 <- traverseEither(parents.reverse)(validateContextValue(subgraph, _, selections, argumentType))
        } yield SourceField(subgraph.name, parentName, field.name) -> ContextArgument(argument.name, name, selections)
        result.left.map(error =>
          s"[${subgraph.name}] Invalid Federation @fromContext application at '$coordinate': $error"
        )
      }

      val declarations = contextDirectives.map { case (typeName, directive) =>
        contextDeclaration(subgraph, typeName, directive)
      }
      val bindings     = schema.fields.flatMap { case (typeName, field) =>
        for {
          argument  <- field.args
          directive <- argument.directives
          if names.fromContext.contains(directive.name)
        } yield compileContextArgument(typeName, field, argument, directive)
      }

      (validateAll(declarations), validateAll(bindings)) match {
        case (Right(declared), Right(bound)) =>
          val declaredContexts = declared.groupMap(_._1)(_._2).map { case (sourceType, contexts) =>
            sourceType -> contexts.toSet
          }
          Right(FederationContexts(declaredContexts.toList, bound.groupMap(_._1)(_._2).toList))
        case (declared, bound)               => Left(declared.left.getOrElse(Nil) ::: bound.left.getOrElse(Nil))
      }
    }

  private def validateContextReceiver(source: String, typeName: String, fieldName: String): Either[String, Unit] = {
    val receiver        = types.find(entry => entry.source == source && entry.name == typeName)
    val parent          = receiver.map(_.tpe)
    val implementsField = parent.toList.flatMap(_.interfaces().getOrElse(Nil)).exists { interface =>
      Option(interface.getFieldOrNull(fieldName)).nonEmpty
    }
    val isEntityObject  =
      receiver.exists(_.entity.exists(_.lookups.nonEmpty)) && parent.exists(_.kind == __TypeKind.OBJECT)
    if (implementsField) Left("context arguments cannot be used on fields that implement interface fields.")
    else if (!isEntityObject) Left("the containing object must define a resolvable entity lookup.")
    else Right(())
  }

  private def contextParents(subgraph: PreparedSubgraph, contextTypes: List[String]): Either[String, List[__Type]] =
    traverseEither(contextTypes) { contextType =>
      if (isInterfaceObject(subgraph.name, subgraph.rootNames.composed(contextType)))
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
        values.forall(compatibleContextValueType(_, argumentType)),
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

    def isInterfaceObjectType(tpe: __Type): Boolean = tpe.name.exists(isInterfaceObject(subgraph.name, _))

    def validateFragments(values: List[Selection], topLevel: Boolean): Either[String, Unit] =
      traverseEither(values) {
        case field: Selection.Field             => validateFragments(field.selectionSet, topLevel = false)
        case fragment: Selection.InlineFragment =>
          val condition = fragment.typeCondition.flatMap(value => rootType.types.get(value.name))
          if (!topLevel) Left("inline fragments are only allowed at the top level of a context selection.")
          else if (!condition.exists(_.kind == __TypeKind.OBJECT))
            Left("top-level context type conditions must name concrete object types.")
          else if (condition.exists(isInterfaceObjectType)) Left(InterfaceObjectContextSelection)
          else validateFragments(fragment.selectionSet, topLevel = false)
        case _: Selection.FragmentSpread        => Right(())
      }.map(_ => ())

    def projectType(container: __Type, selected: __Type): __Type =
      contextValueType(container) match {
        case list if list.kind == __TypeKind.LIST => list.copy(ofType = list.ofType.map(projectType(_, selected)))
        case _                                    => selected
      }

    def applies(fragment: Selection.InlineFragment, runtime: __Type): Boolean =
      fragment.typeCondition.forall(condition =>
        rootType.types.get(condition.name).exists(contextPossibleTypes(rootType, _).exists(_.name == runtime.name))
      )

    def resolveField(staticType: __Type, field: Selection.Field): Either[String, List[__Type]] =
      if (field.name == TypenameField && field.selectionSet.isEmpty) Right(Types.string :: Nil)
      else
        Option(staticType.getFieldOrNull(field.name))
          .toRight(s"field '${field.name}' does not exist on context type '${staticType.name.getOrElse("")}'.")
          .flatMap { definition =>
            val output = definition._type.innerType
            if (field.selectionSet.isEmpty) Right(contextValueType(definition._type) :: Nil)
            else if (isInterfaceObjectType(output)) Left(InterfaceObjectContextSelection)
            else
              traverseEither(contextPossibleTypes(rootType, output))(resolve(output, _, field.selectionSet))
                .map(_.flatten.map(projectType(definition._type, _)))
          }

    def resolve(staticType: __Type, runtime: __Type, values: List[Selection]): Either[String, List[__Type]] = {
      val selected = values.filter {
        case _: Selection.Field                 => true
        case fragment: Selection.InlineFragment => applies(fragment, runtime)
        case _: Selection.FragmentSpread        => false
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

    for {
      _      <- Either.cond(!isInterfaceObjectType(parent), (), InterfaceObjectContextSelection)
      _      <- validateFragments(selections, topLevel = true)
      values <- traverseEither(contextPossibleTypes(rootType, parent))(resolve(parent, _, selections))
    } yield values.flatten
  }

}

private[composition] object ContextCompilation {
  final case class FederationContexts(
    declaredContexts: List[(SourceType, Set[ContextName])],
    contextBindings: List[(SourceField, List[ContextArgument])]
  )

  def parseSelection(value: String): Option[(ContextName, List[Selection])] = {
    def indexOrEnd(index: Int): Int = if (index < 0) value.length else index

    @tailrec def skipIgnored(index: Int): Int =
      if (index >= value.length) index
      else
        value.charAt(index) match {
          case character if character.isWhitespace || character == ',' => skipIgnored(index + 1)
          case '#'                                                     =>
            skipIgnored(indexOrEnd(value.indexWhere(character => character == '\n' || character == '\r', index + 1)))
          case _                                                       => index
        }

    val dollar = skipIgnored(0)
    if (dollar >= value.length || value.charAt(dollar) != '$') None
    else {
      val start = skipIgnored(dollar + 1)
      val end   = indexOrEnd(value.indexWhere(character => !(character.isLetterOrDigit || character == '_'), start))
      val name  = value.substring(start, end)
      if (!isContextName(name)) None
      else {
        val rawSelections = value.substring(skipIgnored(end))
        val parsed        =
          if (rawSelections.startsWith("{")) parseSelectionSet(s"query $rawSelections")
          else parseFieldSet(rawSelections)
        parsed.map(ContextName(name) -> _)
      }
    }
  }

  def diagnostic(
    names: FederationDirectiveNames,
    application: TypeSystemDirectiveApplication,
    directiveName: String
  ): Option[String] = {
    val coordinate    = application.coordinate.display
    val isContext     = names.context.contains(directiveName)
    val isFromContext = names.fromContext.contains(directiveName)
    if ((isContext || isFromContext) && !names.supportsContexts)
      Some(
        s"Federation ${if (isContext) "@context" else "@fromContext"} is not available in the linked feature version at '$coordinate'."
      )
    else if (isContext && !application.supportsContext)
      Some(s"Federation @context is not supported at '$coordinate'.")
    else if (isFromContext && !application.supportsFromContext)
      Some(s"Federation @fromContext is not supported at '$coordinate'.")
    else None
  }

  private val ContextNamePattern              = raw"[A-Za-z][A-Za-z0-9]*".r
  private val InterfaceObjectContextSelection = "context selections cannot reference an @interfaceObject type."

  private def isContextName(name: String): Boolean = ContextNamePattern.pattern.matcher(name).matches()

  private def contextDeclaration(
    subgraph: PreparedSubgraph,
    typeName: String,
    directive: Directive
  ): Either[String, (SourceType, ContextName)] =
    directive.arguments.get("name") match {
      case Some(StringValue(name)) if isContextName(name) =>
        subgraph.rootNames.composed(typeName) match {
          case operation @ ("Mutation" | "Subscription") =>
            Left(s"[${subgraph.name}] Federation @context is not supported on the $operation root type '$typeName'.")
          case composedName                              => Right(SourceType(subgraph.name, composedName) -> ContextName(name))
        }
      case Some(StringValue(name))                        =>
        Left(s"[${subgraph.name}] Invalid Federation @context name '$name' on '$typeName'.")
      case _                                              =>
        Left(s"[${subgraph.name}] Invalid Federation @context application on '$typeName'.")
    }

  private def validateContextSelectionSyntax(selections: List[Selection]): Either[String, Unit] =
    selections.foldLeft[Either[String, Unit]](Right(())) {
      case (result, Selection.Field(alias, _, _, directives, children, _)) =>
        result.flatMap(_ =>
          if (alias.nonEmpty) Left("aliases are not allowed in a context selection.")
          else if (directives.nonEmpty) Left("directives are not allowed in a context selection.")
          else validateContextSelectionSyntax(children)
        )
      case (result, Selection.InlineFragment(_, directives, children))     =>
        result.flatMap(_ =>
          if (directives.nonEmpty) Left("directives are not allowed in a context selection.")
          else validateContextSelectionSyntax(children)
        )
      case (_, _: Selection.FragmentSpread)                                =>
        Left("fragment spreads are not allowed in a context selection.")
    }

  private def validateContextTypeConditions(
    rootType: RootType,
    locations: List[__Type],
    selections: List[Selection]
  ): Either[String, Unit] = {
    val locationTypes = locations.iterator.flatMap(contextPossibleTypes(rootType, _)).flatMap(_.name).toSet

    def matchesNoLocation(condition: String): Boolean =
      rootType.types
        .get(condition)
        .forall(tpe => contextPossibleTypes(rootType, tpe).flatMap(_.name).forall(!locationTypes(_)))

    val unused = selections.collect {
      case Selection.InlineFragment(Some(condition), _, _) if matchesNoLocation(condition.name) => condition.name
    }.distinct.sorted
    Either.cond(
      unused.isEmpty,
      (),
      s"top-level context type conditions do not match a context location: ${unused.mkString(", ")}."
    )
  }

  private def contextPossibleTypes(rootType: RootType, tpe: __Type): List[__Type] =
    if (!isAbstractType(tpe)) tpe :: Nil
    else {
      val direct = tpe.possibleTypes.getOrElse(Nil)
      if (direct.nonEmpty) direct
      else
        rootType.types.valuesIterator.filter { candidate =>
          candidate.kind == __TypeKind.OBJECT && candidate.interfaces().getOrElse(Nil).exists(_.name == tpe.name)
        }.toList
    }

  private def contextValueType(tpe: __Type): __Type =
    tpe.kind match {
      case __TypeKind.NON_NULL => tpe.ofType.fold(tpe)(contextValueType)
      case __TypeKind.LIST     => tpe.copy(ofType = tpe.ofType.map(contextValueType))
      case _                   => tpe
    }

  private def compatibleContextValueType(selected: __Type, argument: __Type): Boolean = {
    val selectedValue = contextValueType(selected)
    val argumentValue = contextValueType(argument)
    (selectedValue.kind, argumentValue.kind) match {
      case (__TypeKind.LIST, __TypeKind.LIST) =>
        (selectedValue.ofType, argumentValue.ofType) match {
          case (Some(selectedItem), Some(argumentItem)) => compatibleContextValueType(selectedItem, argumentItem)
          case _                                        => false
        }
      case _                                  =>
        selectedValue.kind == argumentValue.kind && selectedValue.name == argumentValue.name
    }
  }

}
