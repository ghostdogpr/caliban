package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph.{ rootName, ProgressiveRoute, Source }
import caliban.gateway.internal.composition.TypeComposition._
import caliban.introspection.adt._
import caliban.parsing.adt.OperationType

import scala.collection.compat._

/**
 * Checks and merges type declarations, including operation roots and recursive type references.
 */
private[composition] final class TypeComposition(
  types: List[SubgraphType],
  directives: DirectiveComposition.ComposedDirectives
) {
  import DirectiveComposition._

  lazy val diagnostics: List[String] =
    check(!inaccessibleTypes("Query"), "[type Query] The query root type cannot be @inaccessible.") :::
      groups.flatMap { case group @ TypeGroup(name, entries) =>
        entries.map(_.tpe.kind).distinct match {
          case (__TypeKind.OBJECT | __TypeKind.INTERFACE) :: Nil => objectDiagnostics(group)
          case __TypeKind.INPUT_OBJECT :: Nil                    => inputDiagnostics(name, entries)
          case __TypeKind.ENUM :: Nil                            => enumDiagnostics(name, entries)
          case __TypeKind.SCALAR :: Nil                          => scalarDiagnostics(name, entries)
          case _ :: _ :: _                                       =>
            List(s"[type $name] Kinds are incompatible between subgraphs: ${sources(entries)}.")
          case _                                                 => Nil
        }
      }

  lazy val composed: Map[String, __Type] = mergeTypes

  private lazy val groups = groupNonEmpty(types)(_.name).toList.map { case (name, entries) => TypeGroup(name, entries) }

  private lazy val inputTypeNames: Set[String] = types.iterator.flatMap { entry =>
    entry.tpe.allInputFields.iterator.flatMap(_._type.innerType.name) ++
      entry.tpe.allFields.iterator.flatMap(_.allArgs.iterator.flatMap(_._type.innerType.name))
  }.toSet

  private lazy val outputTypeNames: Set[String] =
    types.iterator.flatMap(_.tpe.allFields.iterator.flatMap(_._type.innerType.name)).toSet

  private lazy val interfaceObjects =
    types.filter(entry => entry.subgraph.isInterfaceObject(entry.name)).map(_.name).toSet

  private lazy val inaccessibleTypes = directives.hidden.collect { case TypeCoordinate(name, _) => name }

  private def hasInaccessibleType(tpe: __Type): Boolean = tpe.innerType.name.exists(inaccessibleTypes)

  private def fieldEntries: Iterator[(FieldCoordinate, List[FieldEntry])] =
    groups.iterator.flatMap { group =>
      group.fields.map { case (field, entries) => FieldCoordinate(group.name, field) -> entries }
    }

  // Routes with every progressive override label inactive, and the routes of fields whose label is active.
  lazy val inactiveRoutes: Map[FieldCoordinate, List[Source]] =
    fieldEntries.flatMap { case (field, entries) =>
      val sources = owners(entries, withLabels = false)
      if (sources.isEmpty) None else Some(field -> sources.map(_.owner.subgraph).distinct.sorted)
    }.toMap

  lazy val progressiveRoutes: Map[FieldCoordinate, ProgressiveRoute] =
    fieldEntries.flatMap { case (field, entries) =>
      entries.flatMap(_.field.overrideDirective).flatMap(_.progressive).headOption.map { progressive =>
        field -> ProgressiveRoute(progressive, owners(entries, withLabels = true).map(_.owner.subgraph).distinct.sorted)
      }
    }.toMap

  private def hiddenArguments(typeName: String, fieldName: String): String => Boolean =
    argument => directives.hidden(ArgumentCoordinate(typeName, fieldName, argument))

  private def objectDiagnostics(group: TypeGroup): List[String] = {
    val TypeGroup(name, entries) = group
    val operation                = RootOperations.get(name)
    val abstracted               = interfaceObjectFields(entries)
    group.fields.toList.flatMap { case (fieldName, values) =>
      val fieldPath          = if (operation.isEmpty) s"$name.$fieldName" else s"${name.toLowerCase}.$fieldName"
      val resolving          = values ::: abstracted.getOrElse(fieldName, Nil)
      val owned              = owners(resolving, withLabels = true)
      val ownerTypes         = owned.map(_.owner)
      val unshared           =
        owned.filter(value =>
          value.owner.subgraph.directiveNames.mode == SubgraphMode.Federation2 && !value.field.shareable
        )
      val hiddenArgument     = hiddenArguments(name, fieldName)
      val compatible         = fieldsCompatible(values.map(value => visibleArguments(value.field.definition, hiddenArgument)))
      val sharedSubscription = operation.contains(OperationType.Subscription) &&
        (owned.size > 1 || values.exists(_.field.shareable))
      val sharedOrdinary     =
        operation.nonEmpty && compatible && owned.size > 1 && ownerTypes.exists(!_.subgraph.federation)
      val sharedUnshareable  = compatible && owned.size > 1 && entries.exists(_.tpe.kind == __TypeKind.OBJECT) &&
        unshared.nonEmpty && (operation.isEmpty || ownerTypes.forall(_.subgraph.federation))
      val prefix             = if (operation.isEmpty) s"[type $fieldPath]" else s"[$fieldPath]"
      overrideDiagnostics(prefix, values, resolving) ::: contextualArgumentDiagnostics(fieldPath, values) :::
        check(
          !sharedSubscription,
          s"$prefix Subscription fields require one effective owner and cannot be @shareable."
        ) :::
        check(!sharedOrdinary, s"$prefix Field is resolved by multiple ordinary subgraphs: ${sources(ownerTypes)}.") :::
        check(
          compatible,
          s"$prefix Definitions are incompatible between subgraphs: ${sources(values.map(_.owner))}."
        ) :::
        check(
          !sharedUnshareable,
          s"$prefix Field is resolved by multiple subgraphs without compatible @shareable declarations: ${sources(ownerTypes)}."
        ) :::
        (if (inaccessibleTypes(name) || directives.hidden(FieldCoordinate(name, fieldName))) Nil
         else visibilityDiagnostics(fieldPath, values, hiddenArgument))
    }
  }

  // Fields that an @interfaceObject of one of the types' interfaces resolves for them in another subgraph.
  private def interfaceObjectFields(entries: List[SubgraphType]): Map[String, List[FieldEntry]] = {
    val interfaces = entries.flatMap(_.tpe.interfaces().getOrElse(Nil)).flatMap(_.name).toSet
    types
      .filter(tpe => interfaces(tpe.name) && tpe.subgraph.isInterfaceObject(tpe.name))
      .flatMap(tpe => tpe.fields.map(FieldEntry(tpe, _)))
      .groupBy(_.field.definition.name)
  }

  private def overrideDiagnostics(prefix: String, values: List[FieldEntry], resolving: List[FieldEntry]) = {
    val overrides         = values.flatMap(value => value.field.overrideDirective.map(value -> _))
    val invalid           = overrides.flatMap { case (FieldEntry(entry, field), value) =>
      val name = field.definition.name
      val from = resolving.filter(_.owner.source == value.from)
      check(entry.source != value.from, s"$prefix Subgraph '${entry.source}' cannot @override itself.") :::
        check(
          value.progressive.isEmpty || from.exists(_.field.owned),
          s"$prefix Progressive @override in subgraph '${entry.source}' requires its 'from' subgraph '${value.from}' to own the field."
        ) :::
        check(
          entry.tpe.kind != __TypeKind.INTERFACE,
          s"[${entry.source}] Federation @override is not supported at '${entry.name}.$name'."
        ) :::
        from.collectFirst {
          case resolved if resolved.owner.name != entry.name                                   => value.from -> "@interfaceObject"
          case resolved if resolved.owner.subgraph.requiredFieldSet(entry.name, name).nonEmpty =>
            value.from -> "@requires"
          case resolved if resolved.owner.subgraph.providedFieldSet(entry.name, name).nonEmpty =>
            value.from -> "@provides"
        }
          .orElse(if (field.owned) None else Some(entry.source -> "@external"))
          .map { case (subgraph, directive) =>
            s"$prefix @override from '${value.from}' conflicts with $directive on the field in '$subgraph'."
          }
          .toList
    }
    val overridingSources = overrides.map(_._1.owner.source).distinct.sorted
    invalid ::: check(
      overridingSources.size <= 1,
      s"$prefix Subgraphs ${formatSources(overridingSources)} declare @override for the field."
    )
  }

  private def visibilityDiagnostics(
    fieldPath: String,
    values: List[FieldEntry],
    hidden: String => Boolean
  ): List[String] =
    values.flatMap { case FieldEntry(entry, field) =>
      check(
        !hasInaccessibleType(field.definition._type),
        s"[${entry.source}] Field '$fieldPath' must be @inaccessible because its return type is inaccessible."
      ) ::: field.definition.allArgs.flatMap(argument =>
        inputVisibility(entry.source, "argument", s"$fieldPath.${argument.name}", argument, hidden(argument.name))
      )
    }

  private def inputVisibility(source: String, kind: String, path: String, input: __InputValue, hidden: Boolean) =
    if (!hidden && hasInaccessibleType(input._type))
      Some(s"[$source] ${kind.capitalize} '$path' must be @inaccessible because its input type is inaccessible.")
    else if (hidden && isRequiredInput(input))
      Some(s"[$source] Required @inaccessible $kind '$path' must define a default value.")
    else None

  private def inputDiagnostics(name: String, entries: List[SubgraphType]): List[String] = {
    val sourceNames = entries.map(_.source).toSet
    val fields      = entries.flatMap(entry => entry.tpe.allInputFields.map(entry -> _)).groupBy(_._2.name)

    fields.toList.flatMap { case (fieldName, values) =>
      val signatures    = values.map(value => inputSignature(value._2)).distinct
      val omittedFrom   = sourceNames -- values.map(_._1.source)
      val required      = values.exists(value => isRequiredInput(value._2))
      val inaccessible  = directives.hidden(InputFieldCoordinate(name, fieldName))
      val compatibility =
        if (signatures.size > 1)
          List(
            s"[type $name.$fieldName] Input field definitions are incompatible between subgraphs: ${sources(values.map(_._1))}."
          )
        else if (omittedFrom.nonEmpty && required)
          omittedFrom.toList.sorted.map(source =>
            s"[$source] Required input field '$name.$fieldName' is not declared by this subgraph."
          )
        else Nil
      val visibility    =
        if (inaccessibleTypes(name)) Nil
        else
          values.flatMap { case (entry, field) =>
            inputVisibility(entry.source, "input field", s"$name.$fieldName", field, inaccessible)
          }
      compatibility ::: visibility
    }
  }

  private def enumDiagnostics(name: String, entries: List[SubgraphType]): List[String] =
    if (!inputTypeNames(name) || !outputTypeNames(name)) Nil
    else {
      val valueSets =
        entries.map(
          _.tpe.allEnumValues.map(_.name).filterNot(value => directives.hidden(EnumValueCoordinate(name, value))).toSet
        )
      check(
        valueSets.distinct.size <= 1,
        s"[type $name] Input/output enum values are incompatible between subgraphs: ${sources(entries)}."
      )
    }

  private def scalarDiagnostics(name: String, entries: List[SubgraphType]): List[String] =
    check(
      entries.map(entry => scalarSignature(entry.tpe)).distinct.size <= 1,
      s"[type $name] Definitions are incompatible between subgraphs: ${sources(entries)}."
    )

  private def mergeTypes: Map[String, __Type] = {
    val chosen  = groups.collect {
      case group @ TypeGroup(name, entries) if !inaccessibleTypes(name) =>
        // Type references are rewritten when their field thunks run, after composed is initialized.
        val rewrite = rewriteType(_: __Type, composed)
        val shell   = directives.attachType(entries.head.tpe, name)
        val merged  = shell.kind match {
          case __TypeKind.OBJECT | __TypeKind.INTERFACE => mergeObject(group, shell, rewrite)
          case __TypeKind.UNION                         =>
            shell.copy(possibleTypes = Some(entries.flatMap(_.tpe.possibleTypes.getOrElse(Nil))))
          case __TypeKind.INPUT_OBJECT                  => mergeInputObject(name, entries, shell, rewrite)
          case __TypeKind.ENUM                          => mergeEnum(name, entries, shell)
          case _                                        => shell
        }
        name -> merged
    }.toMap
    val objects = chosen.filter(_._2.kind == __TypeKind.OBJECT)
    chosen.map { case (name, tpe) =>
      name -> (if (isAbstractType(tpe)) tpe.copy(possibleTypes = tpe.possibleTypes.map(resolve(_, objects))) else tpe)
    }
  }

  private def mergeObject(group: TypeGroup, shell: __Type, rewrite: __Type => __Type): __Type = {
    val TypeGroup(typeName, entries) = group
    val fields                       = group.fields.toList.sortBy(_._1).flatMap { case (fieldName, values) =>
      val visible   = if (directives.hidden(FieldCoordinate(typeName, fieldName))) Nil else values
      val effective = owners(visible, withLabels = true)
      // Progressive overrides keep both sources available for routing.
      (effective ::: owners(visible, withLabels = false)).distinct match {
        case FieldEntry(_, field) :: rest if effective.nonEmpty =>
          rest
            .foldLeft(Option(field.definition._type))((merged, next) =>
              merged.flatMap(mergeOutputType(_, next.field.definition._type))
            )
            .map(mergedType =>
              directives.attachField(
                typeName,
                sanitizeField(
                  field.definition.copy(`type` = () => mergedType),
                  rewrite,
                  hiddenArguments(typeName, fieldName)
                )
              )
            )
        case _                                                  => None
      }
    }
    val interfaces                   = entries.flatMap(_.tpe.interfaces().getOrElse(Nil))
    val possibleTypes                = entries.flatMap(_.tpe.possibleTypes.getOrElse(Nil))
    lazy val all                     =
      if (shell.kind != __TypeKind.OBJECT) fields
      else {
        val existing = fields.map(_.name).toSet
        fields ::: resolve(interfaces.filter(_.name.exists(interfaceObjects)), composed)
          .flatMap(_.allFields)
          .filterNot(field => existing(field.name))
      }

    shell.copy(
      fields = args => Some(includeDeprecated(all, args.includeDeprecated)(_.isDeprecated)),
      interfaces = () => Some(resolve(interfaces, composed)),
      possibleTypes = if (shell.kind == __TypeKind.INTERFACE) Some(possibleTypes) else shell.possibleTypes
    )
  }

  private def mergeInputObject(
    typeName: String,
    entries: List[SubgraphType],
    shell: __Type,
    rewrite: __Type => __Type
  ): __Type = {
    def common(name: String) = entries.forall(entry => inputFieldDefinition(entry.tpe, name).nonEmpty)
    // A common field must exist in the first declaration.
    val fields               = shell.allInputFields
      .filter(field => common(field.name) && !directives.hidden(InputFieldCoordinate(typeName, field.name)))
      .sortBy(_.name)
      .map(field => directives.attachInputField(typeName, field.copy(`type` = () => rewrite(field._type))))
    shell.copy(inputFields = args => Some(includeDeprecated(fields, args.includeDeprecated)(_.isDeprecated)))
  }

  private def mergeEnum(typeName: String, entries: List[SubgraphType], shell: __Type): __Type = {
    val visible =
      entries.map(_.tpe.allEnumValues.filterNot(value => directives.hidden(EnumValueCoordinate(typeName, value.name))))
    val values  = visible.flatten
      .distinctBy(_.name)
      .filter(value => !inputTypeNames(typeName) || visible.forall(_.exists(_.name == value.name)))
      .sortBy(_.name)
      .map(directives.attachEnumValue(typeName, _))
    shell.copy(enumValues = args => Some(includeDeprecated(values, args.includeDeprecated)(_.isDeprecated)))
  }

}

private[composition] object TypeComposition {
  final case class SubgraphType(
    subgraph: Source,
    name: String,
    tpe: __Type,
    fields: List[SubgraphField]
  ) {
    def source: String = subgraph.name
  }

  final case class SubgraphField(
    definition: __Field,
    owned: Boolean,
    shareable: Boolean,
    overrideDirective: Option[FieldOverride],
    contextualArguments: Set[String]
  )

  final case class FieldEntry(owner: SubgraphType, field: SubgraphField)

  final case class TypeGroup(name: String, entries: ::[SubgraphType]) {
    lazy val fields: Map[String, List[FieldEntry]] =
      entries.flatMap(entry => entry.fields.map(FieldEntry(entry, _))).groupBy(_.field.definition.name)
  }

  val RootOperations: Map[String, OperationType] =
    List(OperationType.Query, OperationType.Mutation, OperationType.Subscription).map(op => rootName(op) -> op).toMap

  sealed trait SubgraphMode
  object SubgraphMode {
    case object Ordinary    extends SubgraphMode
    case object Federation1 extends SubgraphMode
    case object Federation2 extends SubgraphMode
  }

  final case class FieldOverride(from: String, progressive: Option[ComposedGraph.OverrideLabel])

  def rewriteType(tpe: __Type, types: => Map[String, __Type]): __Type =
    tpe.mapInnerType(named => named.name.flatMap(types.get).getOrElse(named))

  private def resolve(references: List[__Type], candidates: Map[String, __Type]): List[__Type] =
    references.flatMap(_.name).distinct.sorted.flatMap(candidates.get)

  def includeDeprecated[A](values: List[A], include: Option[Boolean])(isDeprecated: A => Boolean): List[A] =
    if (include.getOrElse(false)) values else values.filterNot(isDeprecated)

  private def contextualArgumentDiagnostics(
    fieldPath: String,
    values: List[FieldEntry]
  ): List[String] = {
    val contextual = values.flatMap(_.field.contextualArguments).toSet
    contextual.toList.sorted.flatMap { argumentName =>
      values.collect {
        case FieldEntry(entry, field)
            if !field.contextualArguments.contains(argumentName) && field.definition.allArgs
              .exists(argument => argument.name == argumentName && isRequiredInput(argument)) =>
          s"[${entry.source}] Argument '$fieldPath($argumentName:)' must be nullable or define a default value because it is supplied by @fromContext in another subgraph."
      }
    }
  }

  private def owners(entries: List[FieldEntry], withLabels: Boolean): List[FieldEntry] = {
    val claiming   = entries.filter(_.field.owned)
    val owned      = if (withLabels) claiming else claiming.filter(_.field.overrideDirective.forall(_.progressive.isEmpty))
    val overridden = owned.flatMap(_.field.overrideDirective).map(_.from).toSet
    owned.filterNot(entry => overridden.contains(entry.owner.source))
  }

  private def compatibleField(left: __Field, right: __Field): Boolean = {
    def signatures(field: __Field) =
      field.allArgs.map(argument => argument.name -> DirectiveComposition.inputSignature(argument)).toMap
    mergeOutputType(left._type, right._type).nonEmpty && signatures(left) == signatures(right)
  }

  // Possible-type overlap is not transitive, so every pair must be checked.
  private def fieldsCompatible(fields: List[__Field]): Boolean =
    fields.tails.collect { case left :: rest => rest.forall(compatibleField(left, _)) }.forall(identity)

  private def mergeOutputType(left: __Type, right: __Type): Option[__Type] =
    (left.kind, right.kind) match {
      case (__TypeKind.NON_NULL, __TypeKind.NON_NULL) | (__TypeKind.LIST, __TypeKind.LIST) =>
        for { a <- left.ofType; b <- right.ofType; merged <- mergeOutputType(a, b) } yield left.copy(ofType =
          Some(merged)
        )
      case (__TypeKind.NON_NULL, _)                                                        => left.ofType.flatMap(mergeOutputType(_, right))
      case (_, __TypeKind.NON_NULL)                                                        => right.ofType.flatMap(mergeOutputType(left, _))
      case (__TypeKind.LIST, _) | (_, __TypeKind.LIST)                                     => None
      case _                                                                               =>
        val leftPossible  = left.possibleTypeNames
        val rightPossible = right.possibleTypeNames
        if (leftPossible.nonEmpty && leftPossible.subsetOf(rightPossible)) Some(right)
        else if (left.name == right.name && left.kind == right.kind || leftPossible.exists(rightPossible.contains))
          Some(left)
        else None
    }

  private def visibleArguments(field: __Field, hidden: String => Boolean): __Field =
    field.copy(args = args => field.args(args).filterNot(argument => hidden(argument.name)))

  private def sources(entries: List[SubgraphType]): String =
    formatSources(entries.map(_.source))

  private def sanitizeField(field: __Field, rewrite: __Type => __Type, hiddenArgument: String => Boolean): __Field =
    field.copy(
      `type` = () => rewrite(field.`type`()),
      args = args =>
        field.args(args).filterNot(value => hiddenArgument(value.name)).map { value =>
          value.copy(`type` = () => rewrite(value._type))
        }
    )

  private def scalarSignature(tpe: __Type) =
    tpe.specifiedByURL -> DirectiveComposition
      .builtIn(tpe.directives)
      .groupMapReduce(directive =>
        directive.name -> directive.arguments.map { case (name, value) => name -> value.toInputString }
      )(_ => 1)(_ + _)

}
