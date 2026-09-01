package caliban.gateway.internal.composition

import caliban.gateway.internal.composition.TypeComposition._
import caliban.introspection.adt._
import caliban.parsing.adt.Directive

/**
 * Checks and merges the declarations of non-root types, including recursive type references.
 */
private[composition] final class TypeComposition(
  types: List[SubgraphType],
  enumUsages: Map[String, EnumUsage],
  directives: DirectiveComposition.ComposedDirectives
) {
  lazy val diagnostics: List[String] =
    types
      .groupBy(_.name)
      .toList
      .flatMap { case (name, entries) =>
        val kinds = entries.map(_.tpe.kind).distinct
        if (kinds.size > 1)
          List(s"[type $name] Kinds are incompatible between subgraphs: ${sources(entries)}.")
        else
          kinds.headOption.toList.flatMap {
            case __TypeKind.OBJECT | __TypeKind.INTERFACE => incompatibleObjectDiagnostics(name, entries)
            case __TypeKind.INPUT_OBJECT                  => incompatibleInputDiagnostics(name, entries)
            case __TypeKind.ENUM                          =>
              incompatibleEnumDiagnostics(
                name,
                entries,
                enumUsages.getOrElse(name, EnumUsage(input = false, output = false))
              )
            case __TypeKind.SCALAR                        => exactTypeDiagnostics(name, entries)
            case _                                        => Nil
          }
      }

  lazy val composed: Map[String, __Type] = chooseCompatible

  private def incompatibleObjectDiagnostics(name: String, entries: List[SubgraphType]): List[String] = {
    val fields =
      entries.flatMap(entry => entry.tpe.allFields.map(field => field.name -> (entry -> field))).groupBy(_._1)

    fields.toList.flatMap { case (fieldName, definitions) =>
      val values         = definitions.map(_._2)
      val contextualArgs = values.iterator.flatMap { case (entry, _) =>
        entry.contextualArguments.collect { case (`fieldName`, argument) => argument }
      }.toSet
      val contextErrors  = contextualArgumentDiagnostics(
        s"$name.$fieldName",
        values.map { case (entry, field) =>
          (entry.source, field, entry.contextualArguments.collect { case (`fieldName`, argument) => argument })
        }
      )
      val ownedEntries   = effectiveFieldProviders(fieldName, values.map(_._1))
      val owned          = values.filter(value => ownedEntries.exists(_.source == value._1.source))
      val shareable      = owned.nonEmpty && owned.forall { case (entry, _) =>
        !entry.federation2 || entry.shareableFields.contains(fieldName)
      }
      val compatible     = fieldsCompatible(values.map { case (_, field) => visibleArguments(field, contextualArgs) })
      val prefix         = s"[type $name.$fieldName]"
      overrideDiagnostics(
        prefix,
        values.map { case (entry, _) => entry.source -> entry.overrideFields.get(fieldName).map(_.from) }
      ) ::: contextErrors :::
        (if (!compatible) List(s"$prefix Definitions are incompatible between subgraphs: ${sources(values.map(_._1))}.")
         else Nil) :::
        (if (
           compatible && owned.size > 1 && owned.exists(_._1.federation2) &&
           entries.exists(_.tpe.kind == __TypeKind.OBJECT) && !shareable
         )
           List(
             s"$prefix Field is resolved by multiple subgraphs without compatible @shareable declarations: ${sources(owned.map(_._1))}."
           )
         else Nil)
    }
  }

  private def incompatibleInputDiagnostics(name: String, entries: List[SubgraphType]): List[String] = {
    val fields =
      entries.flatMap(entry => entry.tpe.allInputFields.map(field => field.name -> (entry -> field))).groupBy(_._1)

    fields.toList.flatMap { case (fieldName, definitions) =>
      val values      = definitions.map(_._2)
      val signatures  = values.map(value => value._2._type.toType() -> value._2.defaultValue).distinct
      val omittedFrom = entries.map(_.source).toSet -- values.map(_._1.source).toSet
      val required    = values.exists(value => !value._2._type.isNullable && value._2.defaultValue.isEmpty)
      if (signatures.size > 1)
        List(
          s"[type $name.$fieldName] Input field definitions are incompatible between subgraphs: ${sources(values.map(_._1))}."
        )
      else if (omittedFrom.nonEmpty && required)
        omittedFrom.toList.sorted.map(source =>
          s"[$source] Required input field '$name.$fieldName' is not declared by this subgraph."
        )
      else Nil
    }
  }

  private def incompatibleEnumDiagnostics(
    name: String,
    entries: List[SubgraphType],
    usage: EnumUsage
  ): List[String] =
    if (!usage.input || !usage.output) Nil
    else {
      val hiddenNames = entries.iterator.flatMap(_.inaccessibleEnumValues).toSet
      val valueSets   = entries.map(entry => entry.tpe.allEnumValues.map(_.name).toSet -- hiddenNames)
      if (valueSets.distinct.size > 1)
        List(s"[type $name] Input/output enum values are incompatible between subgraphs: ${sources(entries)}.")
      else Nil
    }

  private def exactTypeDiagnostics(name: String, entries: List[SubgraphType]): List[String] = {
    val hidden = hiddenDirectives(entries)
    if (
      entries
        .map(entry => typeSignature(entry.tpe.copy(directives = filterHiddenDirectives(entry.tpe.directives, hidden))))
        .distinct
        .size > 1
    )
      List(s"[type $name] Definitions are incompatible between subgraphs: ${sources(entries)}.")
    else Nil
  }

  private def chooseCompatible: Map[String, __Type] = {
    val inaccessibleTypes = types.iterator.filter(_.inaccessible).map(_.name).toSet
    val chosen            = types
      .groupBy(_.name)
      .flatMap { case (name, entries) =>
        val sorted = entries.sortBy(_.source)
        if (entries.exists(_.inaccessible)) None
        else
          sorted.headOption.map { base =>
            val rewrite = rewriteType(_: __Type, composed)
            val chosen  = base.tpe.kind match {
              case __TypeKind.OBJECT | __TypeKind.INTERFACE =>
                mergeObject(sorted, rewrite, inaccessibleTypes)
              case __TypeKind.UNION                         => mergeUnion(sorted, rewrite, inaccessibleTypes)
              case __TypeKind.INPUT_OBJECT                  => mergeInputObject(sorted, rewrite)
              case __TypeKind.ENUM                          =>
                mergeEnum(
                  sorted,
                  enumUsages.getOrElse(name, EnumUsage(input = false, output = false)),
                  rewrite
                )
              case _                                        =>
                directives.attachType(sanitizeType(base.tpe, rewrite, hiddenDirectives(sorted)), name)
            }
            name -> chosen
          }
      }
    val interfaceObjects  = types.iterator.filter(_.interfaceObject).map(_.name).toSet
    val expanded          = chosen.map { case (name, tpe) =>
      if (tpe.kind != __TypeKind.OBJECT) name -> tpe
      else {
        val inherited = tpe
          .interfaces()
          .getOrElse(Nil)
          .flatMap(_.name)
          .filter(interfaceObjects)
          .flatMap(interfaceName => chosen.get(interfaceName).toList.flatMap(_.allFields))
        val existing  = tpe.allFields.map(_.name).toSet
        val fields    = tpe.allFields ::: inherited.filterNot(field => existing.contains(field.name))
        name -> tpe.copy(fields =
          args => Some(if (args.includeDeprecated.getOrElse(false)) fields else fields.filterNot(_.isDeprecated))
        )
      }
    }
    resolveComposedReferences(expanded)
  }

  private def resolveComposedReferences(mergedTypes: Map[String, __Type]): Map[String, __Type] = {
    def resolve(references: List[__Type], candidates: => Map[String, __Type]): List[__Type] =
      references.flatMap(_.name).distinct.sorted.flatMap(candidates.get)

    lazy val objects: Map[String, __Type]    = mergedTypes.collect {
      case (name, tpe) if tpe.kind == __TypeKind.OBJECT =>
        name -> tpe.copy(interfaces = () => tpe.interfaces().map(resolve(_, resolved)))
    }
    lazy val interfaces: Map[String, __Type] = mergedTypes.collect {
      case (name, tpe) if tpe.kind == __TypeKind.INTERFACE =>
        name -> tpe.copy(
          interfaces = () => tpe.interfaces().map(resolve(_, resolved)),
          possibleTypes = tpe.possibleTypes.map(resolve(_, objects))
        )
    }
    lazy val unions: Map[String, __Type]     = mergedTypes.collect {
      case (name, tpe) if tpe.kind == __TypeKind.UNION =>
        name -> tpe.copy(possibleTypes = tpe.possibleTypes.map(resolve(_, objects)))
    }
    lazy val resolved: Map[String, __Type]   =
      mergedTypes.filterNot { case (_, tpe) =>
        tpe.kind == __TypeKind.OBJECT || tpe.kind == __TypeKind.INTERFACE || tpe.kind == __TypeKind.UNION
      } ++ objects ++ interfaces ++ unions

    resolved
  }

  private def mergeObject(
    entries: List[SubgraphType],
    rewrite: __Type => __Type,
    inaccessibleTypes: Set[String]
  ): __Type = {
    val base          = entries.head.tpe
    val hidden        = hiddenDirectives(entries)
    val fields        = entries
      .flatMap(entry => entry.tpe.allFields.map(field => field.name -> (entry -> field)))
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .flatMap { case (fieldName, definitions) =>
        val values     = definitions.map(_._2)
        val visible    =
          if (values.exists { case (entry, _) => entry.inaccessibleFields.contains(fieldName) }) Nil else values
        val providers  = effectiveFieldProviders(fieldName, visible.map(_._1))
        val ordered    = visible.sortBy { case (entry, _) => (!providers.exists(_.source == entry.source), entry.source) }
        val hiddenArgs = values.iterator.flatMap { case (entry, _) =>
          entry.inaccessibleArguments.collect { case (`fieldName`, argument) => argument }
        }.toSet
        (if (providers.nonEmpty) ordered.headOption else None).map { case (_, field) =>
          val mergedType = ordered.map(_._2._type).reduceOption(mergeOutputType).getOrElse(field._type)
          directives.attachField(
            entries.head.name,
            sanitizeField(field.copy(`type` = () => mergedType), rewrite, hidden, hiddenArgs)
          )
        }
      }
    val interfaces    = mergeReferencedTypes(entries, _.interfaces().getOrElse(Nil), inaccessibleTypes)
    val possibleTypes = mergeReferencedTypes(entries, _.possibleTypes.getOrElse(Nil), inaccessibleTypes)

    directives
      .attachType(sanitizeType(base, rewrite, hidden), entries.head.name)
      .copy(
        fields =
          args => Some(if (args.includeDeprecated.getOrElse(false)) fields else fields.filterNot(_.isDeprecated)),
        interfaces = () => Some(interfaces),
        possibleTypes = if (base.kind == __TypeKind.INTERFACE) Some(possibleTypes) else base.possibleTypes
      )
  }

  private def mergeUnion(
    entries: List[SubgraphType],
    rewrite: __Type => __Type,
    inaccessibleTypes: Set[String]
  ): __Type = {
    val base    = entries.head.tpe
    val hidden  = hiddenDirectives(entries)
    val members = mergeReferencedTypes(entries, _.possibleTypes.getOrElse(Nil), inaccessibleTypes)
    directives
      .attachType(sanitizeType(base, rewrite, hidden), entries.head.name)
      .copy(possibleTypes = Some(members))
  }

  private def mergeReferencedTypes(
    entries: List[SubgraphType],
    references: __Type => List[__Type],
    inaccessibleTypes: Set[String]
  ): List[__Type] = {
    val values = entries.flatMap(entry => references(entry.tpe))
    values
      .flatMap(_.name)
      .distinct
      .sorted
      .filterNot(inaccessibleTypes)
      .flatMap(name => values.find(_.name.contains(name)))
  }

  private def mergeInputObject(
    entries: List[SubgraphType],
    rewrite: __Type => __Type
  ): __Type = {
    val base        = entries.head.tpe
    val hidden      = hiddenDirectives(entries)
    val hiddenNames = entries.iterator.flatMap(_.inaccessibleInputFields).toSet
    val commonNames =
      entries.map(_.tpe.allInputFields.map(_.name).toSet).reduceOption(_ intersect _).getOrElse(Set.empty)
    val fields      = commonNames.diff(hiddenNames).toList.sorted.flatMap { name =>
      entries.iterator
        .flatMap(_.tpe.allInputFields)
        .find(_.name == name)
        .map { field =>
          val sanitized = field.copy(
            `type` = () => rewrite(field._type),
            directives = filterHiddenDirectives(field.directives, hidden)
          )
          directives.attachInputField(entries.head.name, sanitized)
        }
    }
    directives
      .attachType(sanitizeType(base, rewrite, hidden), entries.head.name)
      .copy(
        inputFields =
          args => Some(if (args.includeDeprecated.getOrElse(false)) fields else fields.filterNot(_.isDeprecated))
      )
  }

  private def mergeEnum(
    entries: List[SubgraphType],
    usage: EnumUsage,
    rewrite: __Type => __Type
  ): __Type = {
    val base        = entries.head.tpe
    val hidden      = hiddenDirectives(entries)
    val hiddenNames = entries.iterator.flatMap(_.inaccessibleEnumValues).toSet
    val visible     = entries.map(_.tpe.allEnumValues.filterNot(value => hiddenNames.contains(value.name)))
    val names       =
      if (usage.input) visible.map(_.map(_.name).toSet).reduceOption(_ intersect _).getOrElse(Set.empty)
      else visible.flatMap(_.map(_.name)).toSet
    val values      = names.toList.sorted
      .flatMap(name => visible.iterator.flatten.find(_.name == name))
      .map { value =>
        val sanitized = value.copy(directives = filterHiddenDirectives(value.directives, hidden))
        directives.attachEnumValue(entries.head.name, sanitized)
      }
    directives
      .attachType(sanitizeType(base, rewrite, hidden), entries.head.name)
      .copy(
        enumValues =
          args => Some(if (args.includeDeprecated.getOrElse(false)) values else values.filterNot(_.isDeprecated))
      )
  }

}

private[composition] object TypeComposition {
  final case class SubgraphType(
    source: String,
    name: String,
    tpe: __Type,
    interfaceObject: Boolean,
    entity: Option[EntityDefinition],
    ownedFields: Set[String],
    shareableFields: Set[String],
    inaccessible: Boolean,
    inaccessibleFields: Set[String],
    inaccessibleArguments: Set[(String, String)],
    contextualArguments: Set[(String, String)],
    inaccessibleInputFields: Set[String],
    inaccessibleEnumValues: Set[String],
    overrideFields: Map[String, FieldOverride],
    federation2: Boolean,
    hiddenDirectives: Set[String]
  )

  final case class FieldOverride(
    from: String,
    progressive: Option[ComposedGraph.ProgressiveOverride]
  )

  final case class EntityDefinition(
    keyFields: Set[String],
    lookups: List[ComposedGraph.EntityLookup]
  )

  final case class EnumUsage(input: Boolean, output: Boolean)

  private[composition] def contextualArgumentDiagnostics(
    coordinate: String,
    entries: List[(String, __Field, Set[String])]
  ): List[String] = {
    val contextual = entries.iterator.flatMap(_._3).toSet
    contextual.toList.sorted.flatMap { argumentName =>
      entries.collect {
        case (source, field, contextualArguments)
            if !contextualArguments.contains(argumentName) && field.allArgs.exists(argument =>
              argument.name == argumentName && !argument._type.isNullable && argument.defaultValue.isEmpty
            ) =>
          s"[$source] Argument '$coordinate($argumentName:)' must be nullable or define a default value because it is supplied by @fromContext in another subgraph."
      }
    }
  }

  private[composition] def effectiveFieldProviders(field: String, entries: List[SubgraphType]): List[SubgraphType] = {
    val owned      = entries.filter(_.ownedFields.contains(field))
    val overridden = owned.flatMap(_.overrideFields.get(field).map(_.from)).toSet
    owned.filterNot(entry => overridden.contains(entry.source))
  }

  final case class ProviderOverride(
    from: String,
    by: String,
    progressive: Option[ComposedGraph.ProgressiveOverride]
  )

  private[composition] def interfaceOverrideTargets(
    entries: List[SubgraphType]
  ): Map[(String, String), List[ProviderOverride]] =
    entries.iterator
      .filter(_.tpe.kind == __TypeKind.OBJECT)
      .flatMap { entry =>
        entry.tpe.interfaces().getOrElse(Nil).iterator.flatMap(_.name).flatMap { interfaceName =>
          entry.overrideFields.iterator.map { case (field, overrideDirective) =>
            (interfaceName -> field) -> ProviderOverride(
              overrideDirective.from,
              entry.source,
              overrideDirective.progressive
            )
          }
        }
      }
      .toList
      .groupBy(_._1)
      .map { case (coordinate, values) => coordinate -> values.map(_._2) }

  private[composition] def overrideDiagnostics(
    prefix: String,
    declarations: List[(String, Option[String])]
  ): List[String] = {
    val overrides         = declarations.collect { case (source, Some(from)) => source -> from }
    val invalid           = overrides.collect {
      case (source, from) if source == from => s"$prefix Subgraph '$source' cannot @override itself."
    }
    val overridingSources = overrides.map(_._1).distinct.sorted
    val competing         =
      if (overridingSources.size > 1)
        List(
          s"$prefix Subgraphs ${SchemaComposer.formatSources(overridingSources)} declare @override for the field."
        )
      else Nil
    invalid ::: competing
  }

  private[composition] def compatibleField(left: __Field, right: __Field): Boolean = {
    val leftArgs  = left.allArgs.map(argument => argument.name -> argument).toMap
    val rightArgs = right.allArgs.map(argument => argument.name -> argument).toMap
    compatibleOutputType(left._type, right._type) && leftArgs.keySet == rightArgs.keySet &&
    leftArgs.forall { case (name, argument) =>
      rightArgs
        .get(name)
        .exists(other => argument._type.toType() == other._type.toType() && argument.defaultValue == other.defaultValue)
    }
  }

  private[composition] def fieldsCompatible(fields: List[__Field]): Boolean =
    fields.combinations(2).forall {
      case left :: right :: Nil => compatibleField(left, right)
      case _                    => true
    }

  private[composition] def compatibleOutputType(left: __Type, right: __Type): Boolean =
    (left.kind, right.kind) match {
      case (__TypeKind.NON_NULL, _)                    => left.ofType.exists(compatibleOutputType(_, right))
      case (_, __TypeKind.NON_NULL)                    => right.ofType.exists(compatibleOutputType(left, _))
      case (__TypeKind.LIST, __TypeKind.LIST)          =>
        (left.ofType, right.ofType) match {
          case (Some(a), Some(b)) => compatibleOutputType(a, b)
          case _                  => false
        }
      case (__TypeKind.LIST, _) | (_, __TypeKind.LIST) => false
      case _                                           =>
        val leftPossible  = left.possibleTypeNames
        val rightPossible = right.possibleTypeNames
        left.name == right.name && left.kind == right.kind ||
        leftPossible.nonEmpty && rightPossible.nonEmpty && (leftPossible intersect rightPossible).nonEmpty
    }

  private[composition] def mergeOutputType(left: __Type, right: __Type): __Type =
    (left.kind, right.kind) match {
      case (__TypeKind.NON_NULL, __TypeKind.NON_NULL) =>
        (left.ofType, right.ofType) match {
          case (Some(a), Some(b)) => left.copy(ofType = Some(mergeOutputType(a, b)))
          case _                  => left
        }
      case (__TypeKind.NON_NULL, _)                   => left.ofType.map(mergeOutputType(_, right)).getOrElse(right)
      case (_, __TypeKind.NON_NULL)                   => right.ofType.map(mergeOutputType(left, _)).getOrElse(left)
      case (__TypeKind.LIST, __TypeKind.LIST)         =>
        (left.ofType, right.ofType) match {
          case (Some(a), Some(b)) => left.copy(ofType = Some(mergeOutputType(a, b)))
          case _                  => left
        }
      case _                                          =>
        val leftPossible  = left.possibleTypeNames
        val rightPossible = right.possibleTypeNames
        if (leftPossible.nonEmpty && leftPossible.subsetOf(rightPossible)) right
        else if (rightPossible.nonEmpty && rightPossible.subsetOf(leftPossible)) left
        else left
    }

  private[composition] def hiddenDirectives(entries: List[SubgraphType]): Set[String] =
    entries.iterator.flatMap(_.hiddenDirectives).toSet

  private[composition] def visibleArguments(field: __Field, hidden: Set[String]): __Field =
    if (hidden.isEmpty) field
    else field.copy(args = args => field.args(args).filterNot(argument => hidden.contains(argument.name)))

  private[composition] def sources(entries: List[SubgraphType]): String =
    SchemaComposer.formatSources(entries.map(_.source))

  private[composition] def sanitizeType(tpe: __Type, rewrite: __Type => __Type, hiddenDirectives: Set[String]): __Type =
    tpe.copy(
      directives = filterHiddenDirectives(tpe.directives, hiddenDirectives),
      fields = args => tpe.fields(args).map(_.map(sanitizeField(_, rewrite, hiddenDirectives)))
    )

  private[composition] def sanitizeField(
    field: __Field,
    rewrite: __Type => __Type,
    hiddenDirectives: Set[String],
    inaccessibleArguments: Set[String] = Set.empty
  ): __Field =
    field.copy(
      `type` = () => rewrite(field.`type`()),
      args = args =>
        field.args(args).filterNot(value => inaccessibleArguments.contains(value.name)).map { value =>
          value.copy(
            `type` = () => rewrite(value._type),
            directives = filterHiddenDirectives(value.directives, hiddenDirectives)
          )
        },
      directives = filterHiddenDirectives(field.directives, hiddenDirectives)
    )

  private[composition] def filterHiddenDirectives(
    directives: Option[List[Directive]],
    hiddenDirectives: Set[String]
  ): Option[List[Directive]] =
    if (hiddenDirectives.isEmpty) directives
    else directives.map(_.filterNot(directive => hiddenDirectives.contains(directive.name))).filter(_.nonEmpty)

  private[composition] def rewriteType(tpe: __Type, types: => Map[String, __Type]): __Type =
    tpe.ofType match {
      case Some(ofType) => tpe.copy(ofType = Some(rewriteType(ofType, types)))
      case None         => tpe.name.flatMap(types.get).getOrElse(tpe)
    }

  private[composition] def typeSignature(
    tpe: __Type
  ): (String, Option[String], List[(String, List[(String, String)])]) =
    (
      tpe.name.getOrElse(""),
      tpe.specifiedByURL,
      tpe.directives
        .getOrElse(Nil)
        .map { directive =>
          directive.name -> directive.arguments.toList.map { case (name, value) => name -> value.toInputString }
            .sortBy(_._1)
        }
        .sortBy { case (name, arguments) =>
          name -> arguments.iterator.map { case (key, value) => s"$key=$value" }.mkString
        }
    )

}
