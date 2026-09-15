package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.execution.{ ExecutionRequest, Field }
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan.EntityFetch
import caliban.introspection.adt.{ __InputValue, __Type, __TypeKind }
import caliban.parsing.adt.OperationType
import caliban.Value.{ IntValue, NullValue }

import scala.collection.mutable

private object OperationCost {
  final case class FieldCostParts(oneTime: BigInt, perResult: BigInt)
  final case class SizedPath(path: Vector[String], size: BigInt)
  final case class SizedField(
    parentType: Option[__Type],
    parent: String,
    definitions: List[ComposedGraph.ListSize]
  )
}

/**
 * Estimates the subgraph operations in one query plan using the GraphQL cost specification.
 */
private[gateway] final class OperationCost(
  types: Map[String, __Type],
  runtimeTypesByName: Map[String, Set[String]],
  costs: ComposedGraph.CostMetadata
) {
  import OperationCost.{ FieldCostParts, SizedField, SizedPath }

  private final class FetchMultipliers(
    val representations: Map[Vector[String], BigInt],
    val sizedFields: Map[Vector[String], List[SizedPath]]
  )

  def estimate(request: ExecutionRequest, plan: OperationPlan): Either[String, Long] =
    plan.passthroughSubgraph match {
      case Some(source) =>
        val fields     = collectedFields(request.field)
        val validation = if (costs.listSizes.isEmpty) None else validateListSizes(fields, source)
        validation.toLeft(
          bounded(operationBase(request.operationType) + fieldsCost(fields, source))
        )
      case None         =>
        val validation =
          if (costs.listSizes.isEmpty) None
          else
            (plan.roots.iterator.map(fetch => validateListSizes(fetch.selections, fetch.source)) ++
              plan.entities.iterator.map(fetch => validateListSizes(fetch.fields, fetch.source))).collectFirst {
              case Some(error) => error
            }
        validation.toLeft {
          val multipliers =
            if (costs.listSizes.isEmpty) new FetchMultipliers(Map.empty, Map.empty)
            else representationMultipliers(plan)
          val roots       = plan.roots.foldLeft(BigInt(0)) { (total, fetch) =>
            total + operationBase(plan.operation) + fieldsCost(fetch.selections, fetch.source)
          }
          val entities    = plan.entities
            .groupBy(fetch => fetch.root -> fetch.mergePath)
            .values
            .foldLeft(BigInt(0))((total, fetches) => total + entityFetchesCost(fetches, multipliers))
          bounded(roots + entities)
        }
    }

  private def collectedFields(parent: Field): List[Field] =
    if (parent.allFieldsUniqueNameAndCondition)
      parent.fields.map(field => field.copy(fields = collectedFields(field)))
    else {
      val name         = parent.fieldType.innerType.name.getOrElse("")
      val runtimeTypes = runtimeTypesByName.getOrElse(name, Set(name))
      val fields       = mutable.LinkedHashMap.empty[Field, Set[String]]
      runtimeTypes.toList.sorted.foreach { runtime =>
        parent.collectFields(runtime).foreach { field =>
          val shared = field.copy(_condition = None)
          fields.update(shared, fields.getOrElse(shared, Set.empty) + runtime)
        }
      }
      fields.iterator.map { case (field, members) =>
        val condition = if (members == runtimeTypes) None else Some(members)
        field.copy(fields = collectedFields(field), _condition = condition)
      }.toList
    }

  private def operationBase(operation: OperationType): BigInt =
    if (operation == OperationType.Mutation) BigInt(10) else BigInt(0)

  private def fieldsCost(fields: List[Field], source: String): BigInt =
    conditionalCost(fields, (field: Field) => field._condition)((field, _) => fieldCost(field, source))

  private def entityFetchCost(
    fetch: EntityFetch,
    multipliers: FetchMultipliers,
    runtimeType: Option[String] = None
  ): BigInt = {
    val entity     = outputNamedTypeCost(fetch.entityType).max(BigInt(0))
    val fields     =
      runtimeType.fold(fetch.fields)(runtime => fetch.fields.filter(_._condition.forall(_.contains(runtime))))
    val selections = multipliers.sizedFields.get(fetch.mergePath) match {
      case Some(paths) => sizedFieldsCost(fields, fetch.source, paths)
      case None        => fieldsCost(fields, fetch.source)
    }
    multipliers.representations.getOrElse(fetch.mergePath, BigInt(1)) * (entity + selections)
  }

  private def entityFetchesCost(
    fetches: List[EntityFetch],
    multipliers: FetchMultipliers
  ): BigInt =
    conditionalCost(fetches, entityFetchConditions)((fetch, runtimeType) =>
      entityFetchCost(fetch, multipliers, runtimeType)
    )

  private def entityFetchConditions(fetch: EntityFetch): Option[Set[String]] =
    Some(fetch.fields.flatMap(_._condition.toList.flatten).toSet).filter(_.nonEmpty)

  private def conditionalCost[A](
    values: List[A],
    conditions: A => Option[Set[String]]
  )(cost: (A, Option[String]) => BigInt): BigInt = {
    val (conditional, unconditional) = values.partition(value => conditions(value).nonEmpty)
    val base                         = unconditional.foldLeft(BigInt(0))((total, value) => total + cost(value, None))
    val branch                       = conditional
      .flatMap(value => conditions(value).toList.flatten)
      .distinct
      .map(runtimeType =>
        conditional.foldLeft(BigInt(0)) { (total, value) =>
          if (conditions(value).exists(_.contains(runtimeType))) total + cost(value, Some(runtimeType)) else total
        }
      )
      .reduceOption(_ max _)
      .getOrElse(BigInt(0))
    base + branch
  }

  private def fieldCost(field: Field, source: String): BigInt = {
    val parentType  = field.parentType.map(_.innerType)
    val parent      = parentType.flatMap(_.name).getOrElse("")
    val nested      = fieldsCost(field.fields, source)
    val definitions = listSizes(source, parentType, parent, field.name)
    maximumFieldCost(parentType, parent, field) { own =>
      definitions
        .map(listSizeCost(field, source, own, nested, _))
        .reduceOption(_ max _)
        .getOrElse(own.oneTime + own.perResult + nested)
    }
  }

  private def maximumFieldCost(
    parentType: Option[__Type],
    parent: String,
    field: Field
  )(cost: FieldCostParts => BigInt): BigInt =
    parentType match {
      case Some(tpe) if tpe.kind == __TypeKind.INTERFACE || tpe.kind == __TypeKind.UNION =>
        runtimeTypesByName
          .getOrElse(parent, Set.empty)
          .iterator
          .flatMap(name => types.get(name).flatMap(tpe => Option(tpe.getFieldOrNull(field.name))).map(name -> _))
          .map { case (name, definition) => cost(fieldOwnCost(name, field, definition._type, definition.allArgs)) }
          .reduceOption(_ max _)
          .getOrElse(cost(fieldOwnCost(parent, field, field.fieldType, fieldDefinitions(parentType, field))))
      case _                                                                             =>
        cost(fieldOwnCost(parent, field, field.fieldType, fieldDefinitions(parentType, field)))
    }

  private def listSizeCost(
    field: Field,
    source: String,
    own: FieldCostParts,
    nested: BigInt,
    listSize: ComposedGraph.ListSize
  ): BigInt = {
    val size = resolvedListSize(field, listSize)
    if (listSize.sizedFields.isEmpty) own.oneTime + size * (own.perResult + nested)
    else
      own.oneTime + own.perResult + sizedFieldsCost(
        field.fields,
        source,
        listSize.sizedFields.map(SizedPath(_, size))
      )
  }

  private def sizedFieldsCost(fields: List[Field], source: String, paths: List[SizedPath]): BigInt =
    conditionalCost(fields, (field: Field) => field._condition) { (field, _) =>
      val matching = matchingSizedPaths(field.name, paths)
      if (matching.isEmpty) fieldCost(field, source)
      else if (matching.exists(_.path.isEmpty)) {
        val sized     = sizedField(field, source)
        val local     = declaredSizedPaths(field, sized.definitions)
        val remaining = matching.filter(_.path.nonEmpty)
        val nested    =
          if (local.isEmpty && remaining.isEmpty) fieldsCost(field.fields, source)
          else sizedFieldsCost(field.fields, source, preferSizedPaths(local, remaining))
        val size      = matching.collect { case SizedPath(path, value) if path.isEmpty => value }.max
        maximumFieldCost(sized.parentType, sized.parent, field)(own => own.oneTime + size * (own.perResult + nested))
      } else {
        val sized      = sizedField(field, source)
        val local      = declaredSizedPaths(field, sized.definitions)
        val nested     = sizedFieldsCost(field.fields, source, preferSizedPaths(local, matching))
        val directSize = resolvedDirectListSize(field, sized.definitions)
        maximumFieldCost(sized.parentType, sized.parent, field) { own =>
          directSize.fold(own.oneTime + own.perResult + nested)(size => own.oneTime + size * (own.perResult + nested))
        }
      }
    }

  private def sizedField(field: Field, source: String): SizedField = {
    val parentType = field.parentType.map(_.innerType)
    val parent     = parentType.flatMap(_.name).getOrElse("")
    SizedField(parentType, parent, listSizes(source, parentType, parent, field.name))
  }

  private def preferSizedPaths(primary: List[SizedPath], fallback: List[SizedPath]): List[SizedPath] = {
    val preferred      = maximumSizedPaths(primary)
    val preferredPaths = preferred.iterator.map(_.path).toSet
    preferred ::: maximumSizedPaths(fallback.filterNot(value => preferredPaths.contains(value.path)))
  }

  private def maximumSizedPaths(values: List[SizedPath]): List[SizedPath] =
    values
      .groupBy(_.path)
      .map { case (path, entries) => SizedPath(path, entries.iterator.map(_.size).max) }
      .toList

  private def listSizes(
    source: String,
    parentType: Option[__Type],
    parent: String,
    field: String
  ): List[ComposedGraph.ListSize] = {
    val direct   = costs.listSizes.get((source, parent, field)).toList
    val concrete = parentType.toList
      .filter(tpe => tpe.kind == __TypeKind.INTERFACE || tpe.kind == __TypeKind.UNION)
      .flatMap(_ => runtimeTypesByName.getOrElse(parent, Set.empty))
      .flatMap(name => costs.listSizes.get((source, name, field)))
    (direct ::: concrete).distinct
  }

  private def representationMultipliers(plan: OperationPlan): FetchMultipliers = {
    var result        = Map.empty[Vector[String], BigInt]
    var pendingByPath = Map.empty[Vector[String], List[SizedPath]]

    def record(path: Vector[String], multiplier: BigInt, pending: List[SizedPath]): Unit = {
      result = result.updated(path, result.get(path).fold(multiplier)(_ max multiplier))
      pendingByPath = pendingByPath.updated(path, maximumSizedPaths(pendingByPath.getOrElse(path, Nil) ::: pending))
    }

    def collect(
      fields: List[Field],
      source: String,
      basePath: Vector[String],
      inherited: BigInt,
      pending: List[SizedPath]
    ): Unit =
      fields.foreach { field =>
        val path          = basePath :+ field.aliasedName
        val matching      = matchingSizedPaths(field.name, pending)
        val activated     = matching.filter(_.path.isEmpty).map(_.size).reduceOption(_ max _)
        val parentType    = field.parentType.map(_.innerType)
        val parent        = parentType.flatMap(_.name).getOrElse("")
        val definitions   = listSizes(source, parentType, parent, field.name)
        val direct        = resolvedDirectListSize(field, definitions)
        val multiplier    = inherited * activated.orElse(direct).getOrElse(BigInt(1))
        val localPending  = declaredSizedPaths(field, definitions)
        val nestedPending = preferSizedPaths(
          localPending,
          matching.filter(_.path.nonEmpty)
        )
        record(path, multiplier, nestedPending)
        collect(field.fields, source, path, multiplier, nestedPending)
      }

    plan.roots.foreach(fetch => collect(fetch.selections, fetch.source, Vector.empty, BigInt(1), Nil))
    plan.entities.foreach { fetch =>
      val inherited = result.getOrElse(fetch.mergePath, BigInt(1))
      collect(fetch.fields, fetch.source, fetch.mergePath, inherited, pendingByPath.getOrElse(fetch.mergePath, Nil))
    }
    new FetchMultipliers(result, pendingByPath)
  }

  private def matchingSizedPaths(name: String, paths: List[SizedPath]): List[SizedPath] =
    paths.filter(_.path.headOption.contains(name)).map(value => SizedPath(value.path.drop(1), value.size))

  private def declaredSizedPaths(field: Field, definitions: List[ComposedGraph.ListSize]): List[SizedPath] =
    definitions.flatMap { definition =>
      val size = resolvedListSize(field, definition)
      definition.sizedFields.map(SizedPath(_, size))
    }

  private def validateListSizes(fields: List[Field], source: String): Option[String] =
    fields.iterator.flatMap { field =>
      val parentType = field.parentType.map(_.innerType)
      val parent     = parentType.flatMap(_.name).getOrElse("")
      val current    = listSizes(source, parentType, parent, field.name).iterator.flatMap { listSize =>
        if (listSize.requireOneSlicingArgument && listSize.slicingArguments.nonEmpty) {
          val supplied = listSize.slicingArguments.count(argument => slicingValue(field, argument).nonEmpty)
          if (supplied != 1)
            Some(s"Exactly one slicing argument must be supplied for field '$parent.${field.name}'.")
          else None
        } else None
      }
      current ++ validateListSizes(field.fields, source).iterator
    }.find(_ => true)

  private def resolvedListSize(field: Field, listSize: ComposedGraph.ListSize): BigInt =
    listSize.slicingArguments
      .flatMap(argument => slicingValue(field, argument))
      .reduceOption(_ max _)
      .orElse(listSize.assumedSize.map(value => BigInt(value).max(BigInt(0))))
      .getOrElse(BigInt(1))

  private def resolvedDirectListSize(
    field: Field,
    definitions: List[ComposedGraph.ListSize]
  ): Option[BigInt] =
    definitions
      .filter(_.sizedFields.isEmpty)
      .map(resolvedListSize(field, _))
      .reduceOption(_ max _)

  private def slicingValue(
    field: Field,
    argument: ComposedGraph.SlicingArgument
  ): Option[BigInt] = {
    val path = argument.path

    def nested(value: InputValue, remaining: Vector[String]): Option[InputValue] =
      remaining.headOption match {
        case None       => Some(value)
        case Some(name) =>
          value match {
            case InputValue.ObjectValue(values) => values.get(name).flatMap(nested(_, remaining.tail))
            case _                              => None
          }
      }

    path.headOption
      .flatMap(name => field.arguments.get(name).orElse(argument.defaultValue))
      .flatMap(nested(_, path.drop(1)))
      .flatMap {
        case InputValue.ListValue(values) => Some(BigInt(values.size))
        case NullValue                    => None
        case _ if argument.listValued     => Some(BigInt(1))
        case value: IntValue              => Some(value.toBigInt.max(BigInt(0)))
        case _                            => None
      }
  }

  private def defaultValue(argument: __InputValue): Option[InputValue] =
    argument.parsedDefaultValue

  private def fieldDefinitions(parentType: Option[__Type], field: Field): List[__InputValue] =
    parentType.flatMap(tpe => Option(tpe.getFieldOrNull(field.name))).toList.flatMap(_.allArgs)

  private def fieldOwnCost(
    parent: String,
    field: Field,
    fieldType: __Type,
    definitions: List[__InputValue]
  ): FieldCostParts = {
    val oneTime = costs.fields.get(parent -> field.name).fold(BigInt(0))(BigInt(_)) +
      fieldArguments(parent, field, definitions)
    FieldCostParts(oneTime.max(BigInt(0)), outputTypeCost(fieldType).max(BigInt(0)))
  }

  private def fieldArguments(parent: String, field: Field, arguments: List[__InputValue]): BigInt =
    arguments.foldLeft(BigInt(0)) { case (total, argument) =>
      val name = argument.name
      field.arguments.get(name).orElse(defaultValue(argument)) match {
        case Some(value) =>
          val base = costs.arguments
            .get((parent, field.name, name))
            .fold(inputTypeCost(argument._type))(BigInt(_))
          total + base + inputFieldCost(value, argument._type)
        case None        => total
      }
    }

  private def inputFieldCost(value: InputValue, tpe: __Type): BigInt =
    tpe.kind match {
      case __TypeKind.NON_NULL     => tpe.ofType.fold(BigInt(0))(inputFieldCost(value, _))
      case __TypeKind.LIST         =>
        tpe.ofType.fold(BigInt(0))(element =>
          value match {
            case InputValue.ListValue(values) =>
              values.foldLeft(BigInt(0))((total, value) => total + inputFieldCost(value, element))
            case value                        => inputFieldCost(value, element)
          }
        )
      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(values) =>
            tpe.allInputFields.foldLeft(BigInt(0)) { case (total, field) =>
              values.get(field.name).orElse(defaultValue(field)) match {
                case Some(nested) => total + inputValueCost(tpe, field, nested)
                case None         => total
              }
            }
          case _                              => BigInt(0)
        }
      case _                       => BigInt(0)
    }

  private def inputValueCost(parent: __Type, field: __InputValue, value: InputValue): BigInt = {
    val parentName = parent.name.getOrElse("")
    val base       = costs.inputFields.get(parentName -> field.name).fold(inputTypeCost(field._type))(BigInt(_))
    base + inputFieldCost(value, field._type)
  }

  private def inputTypeCost(tpe: __Type): BigInt = {
    val inner = tpe.innerType
    costs.types.get(inner.name.getOrElse("")).fold(defaultTypeCost(inner))(BigInt(_))
  }

  private def outputTypeCost(tpe: __Type): BigInt = {
    val inner = tpe.innerType
    inner.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION =>
        val concrete = inner.name.toList.flatMap(name => runtimeTypesByName.getOrElse(name, Set.empty))
        concrete
          .map(name => costs.types.get(name).fold(BigInt(1))(BigInt(_)))
          .reduceOption(_ max _)
          .getOrElse(BigInt(1))
      case _                                       =>
        costs.types.get(inner.name.getOrElse("")).fold(defaultTypeCost(inner))(BigInt(_))
    }
  }

  private def outputNamedTypeCost(name: String): BigInt =
    types.get(name).fold(BigInt(1))(outputTypeCost)

  private def defaultTypeCost(tpe: __Type): BigInt =
    tpe.kind match {
      case __TypeKind.SCALAR | __TypeKind.ENUM => BigInt(0)
      case _                                   => BigInt(1)
    }

  private def bounded(value: BigInt): Long =
    if (value > BigInt(Long.MaxValue)) Long.MaxValue
    else if (value < 0) 0L
    else value.longValue
}
