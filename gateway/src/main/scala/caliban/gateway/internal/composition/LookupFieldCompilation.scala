package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.CompositionDiagnostic.{ error, Code }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition.{ ArgumentCoordinate, FieldCoordinate }
import caliban.gateway.internal.composition.FederationCompilation.FederationApplication
import caliban.gateway.internal.composition.FederationCompilation.FederationDirective.{ Is, LookupField }
import caliban.gateway.internal.composition.FieldSelectionMap._
import caliban.introspection.adt._

/**
 * Compiles the `@lookup` fields of a composite source schema, reached from the query root through argument-less
 * fields, into one entity lookup per possible type of the returned type and per combination of the arguments' `|`
 * alternatives.
 */
private[composition] final class LookupFieldCompilation private (subgraph: Source) {
  import LookupFieldCompilation._

  private val schema  = subgraph.mapping.sourceRootType
  private val mapping = subgraph.mapping

  def compile: Compiled = {
    val queryType  = schema.queryType
    lazy val paths = reachable(queryType.name.toList, queryType.name.map(_ -> List.empty[String]).toMap)
    val compiled   = schema.types.toList.sortBy(_._1).flatMap { case (name, parent) =>
      parent.allFields.filter(isLookup).map(compileField(name, paths.get(name), _))
    }
    Compiled(misplacedMaps ::: compiled.flatMap(_.diagnostics), compiled.flatMap(_.lookups), compiled.flatMap(_.maps))
  }

  private def isLookup(field: __Field): Boolean =
    field.directives.getOrElse(Nil).exists(subgraph.directiveNames.is(_, LookupField))

  private def reachable(level: List[String], found: Map[String, List[String]]): Map[String, List[String]] = {
    val next = for {
      (parent, path) <- level.flatMap(name => schema.types.get(name).zip(found.get(name)))
      field          <- parent.allFields if field.allArgs.isEmpty
      returned        = nullableType(field._type)
      name           <- returned.name if returned.kind == __TypeKind.OBJECT && !found.contains(name)
    } yield name -> (path :+ field.name)
    if (next.isEmpty) found else reachable(next.map(_._1).distinct, found ++ next)
  }

  private def compileField(
    parent: String,
    path: Option[List[String]],
    field: __Field
  ): Compiled = {
    val at                                   = SchemaCoordinate.Member(mapping.clientType(parent), mapping.clientField(parent, field.name))
    val returned                             = nullableType(field._type)
    val typeName                             = returned.name.getOrElse("")
    def invalid(code: Code)(message: String) = List(error(code, List(subgraph.name), Some(at))(message))

    def argumentAt(argument: __InputValue): SchemaCoordinate.Argument =
      SchemaCoordinate.Argument(at.typeName, at.memberName, mapping.clientArgument(parent, field.name, argument.name))
    (for {
      _         <- Either.cond(
                     returned.kind != __TypeKind.LIST,
                     (),
                     invalid(Code.LookupReturnsList)(s"Lookup field '${at.render}' must not return a list.")
                   )
      _         <- Either.cond(
                     field.allArgs.nonEmpty,
                     (),
                     invalid(Code.LookupMustHaveArguments)(s"Lookup field '${at.render}' must declare arguments.")
                   )
      arguments <-
        collectErrors(field.allArgs.map(argument => selectionMap(argumentAt(argument), argument).map(argument -> _)))
    } yield {
      val possibleTypes = schema.types.get(typeName).toList.flatMap(_.possibleTypeNames.toList.sorted)
      val alternatives  = arguments.map { case (argument, declared) =>
        declared.fold(List(FieldSelectionMap.field(argument.name)))(_.toList).map(argument -> _)
      }
      val lookups       = for {
        nested      <- path.toList
        combination <- combinations(alternatives)
        entity      <- possibleTypes
        narrowed    <- traverseOption(combination) { case (argument, value) =>
                         narrow(value, admits(entity)).map(narrowed => argument -> clientSelection(narrowed, Some(entity)))
                       }.toList
      } yield mapping.clientType(entity) -> EntityLookup(
        keyFields(narrowed.map(_._2)),
        LookupOperation.Single(
          nested,
          field.name,
          narrowed.map { case (argument, value) =>
            argument.name -> Lookup.Argument.Leaf(KeyArgument(value, argument._type))
          },
          Some(entity).filter(_ != typeName)
        )
      )
      val maps          = arguments.collect { case (argument, Some(value)) =>
        SelectionMapUse(
          argumentAt(argument),
          clientValue(value, Some(typeName)),
          clientInputType(argument._type),
          mapping.clientType(typeName)
        )
      }
      Compiled(Nil, lookups, maps)
    }).fold(Compiled(_, Nil, Nil), identity(_))
  }

  /**
   * The selection map that `@is` declares on the argument, if any.
   */
  private def selectionMap(
    coordinate: SchemaCoordinate.Argument,
    argument: __InputValue
  ): Either[List[CompositionDiagnostic], Option[SelectedValue]] = {
    def invalid(code: Code)(message: String) = List(error(code, List(subgraph.name), Some(coordinate))(message))
    argument.directives
      .getOrElse(Nil)
      .find(subgraph.directiveNames.is(_, Is))
      .flatMap(directive => stringArgument(directive.arguments, "field")) match {
      case None        => Right(None)
      case Some(value) =>
        for {
          parsed <- parse(value).left.map(message =>
                      invalid(Code.IsInvalidSyntax)(
                        s"Invalid @is field selection map on '${coordinate.render}': $message"
                      )
                    )
          _      <- Either.cond(
                      !paths(parsed).exists(_.exists(_.arguments.nonEmpty)),
                      (),
                      invalid(Code.IsFieldsHasArguments)(
                        s"The @is field selection map on '${coordinate.render}' must not supply arguments."
                      )
                    )
        } yield Some(parsed)
    }
  }

  private def misplacedMaps: List[CompositionDiagnostic] =
    subgraph.applications(Is).collect {
      case FederationApplication(at @ ArgumentCoordinate(typeName, fieldName, _), _, _)
          if !subgraph.applied(LookupField, FieldCoordinate(typeName, fieldName)) =>
        error(Code.IsInvalidUsage, List(subgraph.name), at.schemaCoordinate)(
          s"@is on '${at.display}' requires its field to be a @lookup field."
        )
    }

  private def admits(entity: String)(condition: String): Boolean =
    schema.types.get(condition).exists(_.possibleTypeNames.contains(entity))

  private def clientInputType(tpe: __Type): __Type =
    tpe.mapInnerType(named => named.name.map(mapping.clientType).flatMap(subgraph.rootType.types.get).getOrElse(named))

  private def clientValue(value: SelectedValue, typeName: Option[String]): SelectedValue =
    ::(clientSelection(value.head, typeName), value.tail.map(clientSelection(_, typeName)))

  private def clientSelection(selection: Selection, typeName: Option[String]): Selection =
    selection match {
      case Selection.Leaf(steps)             => Selection.Leaf(clientPath(steps, typeName)._1)
      case Selection.ObjectOf(steps, fields) =>
        val (renamed, end) = clientSteps(steps, typeName)
        Selection.ObjectOf(renamed, ::(clientField(fields.head, end), fields.tail.map(clientField(_, end))))
      case Selection.ListOf(steps, element)  =>
        val (renamed, end) = clientSteps(steps, typeName)
        Selection.ListOf(renamed, clientValue(element, end))
    }

  private def clientField(field: ObjectField, typeName: Option[String]): ObjectField =
    field.copy(value = clientValue(field.value, typeName))

  private def clientPath(steps: ::[PathStep], typeName: Option[String]): (::[PathStep], Option[String]) = {
    val step         = steps.head
    val owner        = step.typeCondition.orElse(typeName)
    val next         = owner.flatMap(schema.types.get).flatMap(fieldDefinition(_, step.field)).flatMap(_._type.innerType.name)
    val renamed      = PathStep(
      step.typeCondition.map(mapping.clientType),
      owner.fold(step.field)(mapping.clientField(_, step.field)),
      step.arguments
    )
    val (tail, last) = clientSteps(steps.tail, next)
    ::(renamed, tail) -> last
  }

  private def clientSteps(steps: List[PathStep], typeName: Option[String]): (List[PathStep], Option[String]) =
    steps match {
      case head :: tail => clientPath(::(head, tail), typeName)
      case Nil          => Nil -> typeName
    }
}

private[composition] object LookupFieldCompilation {

  /**
   * Entity lookups by client type name, and the `@is` selection maps to validate once every subgraph is merged.
   */
  final case class Compiled(
    diagnostics: List[CompositionDiagnostic],
    lookups: List[(String, EntityLookup)],
    maps: List[SelectionMapUse]
  )

  final case class SelectionMapUse(
    at: SchemaCoordinate.Argument,
    value: SelectedValue,
    inputType: __Type,
    outputType: String
  )

  def compile(subgraph: Source): Compiled = new LookupFieldCompilation(subgraph).compile

  private def combinations[A](alternatives: List[List[A]]): List[List[A]] =
    alternatives.foldRight(List(List.empty[A]))((options, rest) =>
      for { option <- options; tail <- rest } yield option :: tail
    )
}
